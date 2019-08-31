"""A utility for fetching my recent flashcards."""

import os
from datetime import datetime, timedelta
import logging
from urllib.parse import urlencode

from pymongo import MongoClient, DESCENDING
from pymongo.collection import Collection

logging.basicConfig(level=logging.INFO)
LOGGER = logging.getLogger("com.c13u.cards")

def fetch_cards_by_date(earliest_datetime: datetime, min_num_cards=1):
    client = MongoClient(os.environ["STUDY_BUDDY_MLAB_MONGO_URI"])
    cards_collection: Collection = client["c13u"]["c13u_study_buddy"]

    query = {
        "createdAt": {"$gt": earliest_datetime}, 
        "isPublic": True, 
        "createdById": 1
    }
    
    num_cards = cards_collection.count_documents(query)
    if num_cards < min_num_cards: 
        LOGGER.info((
            f"Found {num_cards} created by {earliest_datetime.isoformat()}. "
            f"Didn't meet the threshold ({min_num_cards})"
        ))
        return None
    
    LOGGER.info(f"Found {num_cards} cards")
    return cards_collection.find(query).sort("createdAt", DESCENDING)

def draft_blog_post(cards, start_date):
    if not cards: return
    
    today = datetime.now()
    draft_filepath = (
        "/Users/dchege711/dchege711.github.io/notebooks/_autogenerated/"
        f"{today.strftime('%Y-%m-%d')}-flashcards.html"
    )
    LOGGER.info(f"Drafting {draft_filepath}...")

    BASE_QUERY_URL = "https://cards.c13u.com/browse?"
    with open(draft_filepath, "w") as fp:
        relevant_cards_query = urlencode({
            "createdById": 1, 
            "createdAt": {
                "$gt": start_date.isoformat(), "$lt": today.isoformat()
            }
        })

        fp.write((
            f"---\n\nlayout: autogenerated\ntitle: Cards from "
            f"{start_date.strftime('%d %b, %Y')} to {today.strftime('%d %b, %Y')}\n"
            f"date: {today.strftime('%Y-%m-%d')}\n\n---\n\n<section>"
            f"These set of flashcards can all be viewed at "
            f"<a href='{BASE_QUERY_URL}{relevant_cards_query}' target='_blank'>"
            "cards.c13u.com/browse <i class='fas fa-fw fa-external-link-alt'></i></a>. "
            "I own the site and I guarantee no funny business. It's more convenient "
            "for me to send you there than to replicate the contents on this page. "
            "\nHope you find something interesting!\n</section>\n<section>"
        ))

        for card in cards:
            # I assume well behaved <blockquotes> and that the first contains 
            # the prompt for the flash card.
            parts = card["descriptionHTML"].split("</blockquote>")
            if len(parts) == 1:
                prompt = ""
            else:
                prompt = parts[0].split("<blockquote>")[1].strip()

            card_query = urlencode({"cardID": card["_id"]})
            fp.write(
                f"""
                <div style="padding: 2%; margin: 2%;">
                    <div class="card-title">
                        <a href="{BASE_QUERY_URL}{card_query}" target="_blank">
                            {card["title"]} <i class='fas fa-fw fa-external-link-alt'></i>
                        </a>
                    </div>
                    <div class="card-text">
                        <p>
                            {prompt}
                        </p>
                    </div>
                    <div class="card-footer">
                        <small><em>Tags: {", ".join(card["tags"].split())}</em></small>
                    </div>
                </div>
                """
            )
        fp.write("</section>")

def main():
    DATE_RECORD_FILEPATH = "/Users/dchege711/dchege711.github.io/src/com.c13u.cards/last_fetch_date.txt"
    try:
        with open(DATE_RECORD_FILEPATH, "r") as fp:
            datetime_str = fp.readline().strip()
            earliest_datetime = datetime.fromisoformat(datetime_str)
    except FileNotFoundError:
        # As a starting condition, I'll use the last 2 weeks
        earliest_datetime: datetime = datetime.now() - timedelta(days=14)

    cards = fetch_cards_by_date(earliest_datetime)
    if cards is None: return
    
    draft_blog_post(cards, earliest_datetime)

    with open(DATE_RECORD_FILEPATH, "w") as fp:
        fp.write(earliest_datetime.isoformat())

if __name__ == "__main__":
    main()