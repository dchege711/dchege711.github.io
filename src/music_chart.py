"""

music_chart.py

Keeps track of my top songs as provided by LastFM

"""

import os
from glob import glob
import sqlite3
import json
from datetime import date
import re

import requests

BASE_URL = "http://ws.audioscrobbler.com/2.0/"
APP_NAME = os.environ["LAST_FM_APP_NAME"]
API_KEY = os.environ["LAST_FM_API_KEY"]
USERNAME = os.environ["LAST_FM_USERNAME"]

conn = sqlite3.connect("c13u_music.db")
conn.row_factory = sqlite3.Row

def initialize_db():
    """
    Initialize the database if it hasn't been configured yet.
    """

    # conn.execute("DROP TABLE IF EXISTS tracks;")
    # conn.execute("DROP TABLE IF EXISTS artists;")

    conn.execute((
        "CREATE TABLE IF NOT EXISTS tracks ( "
        "track_id TEXT PRIMARY KEY, artist_id TEXT, image_url TEXT, "
        "track_name TEXT, rank INT, rank_delta INT, music_brains_id VARCHAR(255));"
    ))

    conn.execute((
        "CREATE TABLE IF NOT EXISTS artists ( "
        "artist_id TEXT PRIMARY KEY, artist_name TEXT, "
        "music_brains_id VARCHAR(255));"
    ))

def fetch_top_tracks():
    """
    Fetch the top tracks over the past 7 days

    @returns List[dict] keyed by `name`, `duration`, `playcount`, `mbid`, `url`, 
    `streamable`, `artist`, `image`, `@attr`

    """
    with requests.Session() as sess:
        sess.headers.update({"user-agent": APP_NAME})
        get_params = {
            "method": "user.gettoptracks", "user": USERNAME, "period": "7day",
            "api_key": API_KEY, "limit": 500, "format": "json"
        }
        tracks = []
        while True:
            res_json = sess.get(BASE_URL, params=get_params).json()
            tracks += res_json["toptracks"]["track"]
            metadata = res_json["toptracks"]["@attr"]
            if int(metadata["page"]) < int(metadata["totalPages"]):
                get_params["page"] = int(metadata["page"]) + 1
            else:
                break

        return tracks

def persist_tracks(tracks):
    """
    Save the tracks into the database.
    """
    last_fm_prefix = "https://www.last.fm/music/"

    for track in tracks:

        try: image_url = track["image"][-1]["#text"]
        except IndexError: image_url = ""

        track["track_id"] = track["url"].replace(last_fm_prefix, "")
        track["artist_id"] = track["artist"]["url"].replace(last_fm_prefix, "")
        track["image_url"] = image_url
        track["track_name"] = track["name"]
        track["rank"] = track["@attr"]["rank"]
        track["music_brains_id"] = track["mbid"]
        existing_row = conn.execute(
            "SELECT * FROM tracks WHERE track_id = ?", [track["track_id"]]
        ).fetchone()

        if existing_row is None:
            conn.execute(
                (
                    "INSERT INTO tracks (track_id, artist_id, image_url, track_name, "
                    "rank, music_brains_id) VALUES (:track_id, :artist_id, "
                    ":image_url, :track_name, :rank, :music_brains_id);"
                ),
                track
            )
        else:
            track["rank_delta"] = existing_row["rank"] - int(track["rank"])
            conn.execute(
                (
                    "UPDATE tracks "
                    "SET rank_delta = :rank_delta, rank = :rank "
                    "WHERE track_id = :track_id;"
                ),
                track
            )

        conn.execute(
            (
                "INSERT INTO artists (artist_id, artist_name, music_brains_id) "
                "VALUES (?, ?, ?) ON CONFLICT DO NOTHING;"
            ),
            [track["artist_id"], track["artist"]["name"], track["artist"]["mbid"]]
        )

    conn.commit()

def dump_top_tracks(k=10):
    """
    Write the top ``k`` tracks to a JSON file
    """
    res = conn.execute(
        (
            "SELECT track_name, artist_name, rank, rank_delta, image_url "
            "FROM tracks, artists WHERE tracks.artist_id = artists.artist_id AND "
            "rank <= ? ORDER BY rank ASC;"
        ), [k]
    )
    tracks = [dict(**x) for x in res.fetchall()]
    with open("../_data/top_songs.json", "w") as output_file:
        json.dump(tracks, output_file, indent=2)

    prvs_filepath = glob("../notebooks/_posts/*-top-10-tracks.html")[0]
    today_str = date.today().strftime("%Y-%m-%d")
    os.rename(
        prvs_filepath, re.sub(r"\d{4}-\d{2}-\d{2}", today_str, prvs_filepath)
    )

if __name__ == "__main__":
    # Only run the script at least 1 day after previous execution time
    prvs_filepath = glob("../notebooks/_posts/*-top-10-tracks.html")[0]
    today_str = date.today().strftime("%Y-%m-%d")
    if today_str not in prvs_filepath:
        initialize_db()
        tracks = fetch_top_tracks()
        persist_tracks(tracks)
        dump_top_tracks()
        print("Processed", len(tracks), "tracks for this week...")
