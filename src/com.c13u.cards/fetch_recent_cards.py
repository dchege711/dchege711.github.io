"""A utility for fetching my recent flashcards."""

import os
from datetime import datetime, timedelta
import logging

from pymongo import MongoClient, DESCENDING
from pymongo.collection import Collection

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
        LOGGER.info(f"Found {num_cards} created by {datetime.isoformat()}. Didn't meet the threshold ({min_num_cards})")
        return None
    
    print("Found", num_cards, "cards")
    return cards_collection.find(query).sort("createdAt", DESCENDING)

def draft_blog_post(cards):
    if not cards: return
    
    today = datetime.now()
    draft_filepath = (
        "/Users/dchege711/dchege711.github.io/notebooks/_drafts/"
        f"{today.strftime('%Y-%m-%d')}-flashcards.html"
    )

    with open(draft_filepath, "w") as fp:
        fp.write((
            f"---\nlayout: post\ntitle: Cards - {today.strftime('%d %b, %Y')}\n"
            f"{today.strftime('%Y-%m-%d')}"
        ))
        for card in cards:
            fp.write(
                f"""
                <div class="card">
                    <div class="card-title">
                        <a href="#">{card["title"]}</a>
                    </div>
                    <div class="card-text">
                    </div>
                    <div class="card-footer">
                        <p>Created on {card["createdAt"].strftime('%d %b, %Y')}</p>
                        <p>Tags: {", ".join(card["tags"].split())}</p>
                    </div>
                </div>
                """
            )
    

def main():
    DATE_RECORD_FILEPATH = "/Users/dchege711/dchege711.github.io/src/com.c13u.cards/last_fetch_date.txt"
    try:
        with open(DATE_RECORD_FILEPATH, "r") as fp:
            datetime_str = fp.readline().strip()
            earliest_datetime = datetime.fromisoformat()
    except FileNotFoundError:
        # As a starting condition, I'll use the last 1 week
        earliest_datetime: datetime = datetime.now() - timedelta(days=7)

    cards = fetch_cards_by_date(earliest_datetime)
    if cards is None: return
    
    draft_blog_post(cards)

    with open(DATE_RECORD_FILEPATH, "w") as fp:
        fp.write(earliest_datetime.isoformat())

if __name__ == "__main__":
    main()
