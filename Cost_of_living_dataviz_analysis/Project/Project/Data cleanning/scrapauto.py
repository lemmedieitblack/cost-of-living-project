import pandas as pd
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from bs4 import BeautifulSoup
import time
import re
import json
import urllib.parse

def build_url(page_num):
    query = {
        "category": "1",
        "page": str(page_num),
        "sort": "latest",
        "layout": "list",
        "user": {"dealer": "0", "official": "0", "id": ""},
        "year": {"gt": "1911", "lt": "2026"},
        "usdprice": {"gt": "0", "lt": "100000000"},
        "mileage": {"gt": "10", "lt": "1000000"}
    }
    return f"https://www.auto.am/search/passenger-cars?q={urllib.parse.quote(json.dumps(query))}"

def parse_details(details_text):
    parts = details_text.split(",")
    parts = [p.strip() for p in parts if p.strip() != ""]

    running_match = re.match(r"([\d\s]+)(km|miles)", parts[0]) if parts else None
    running = running_match.group(0).strip() if running_match else ""

    type_ = transmission = wheel = motor = color = ""

    for part in parts[1:]:
        if "automatic" in part.lower() or "manual" in part.lower():
            transmission = part
        elif part.lower() in ["left", "right"]:
            wheel = part
        elif any(fuel in part.lower() for fuel in ["petrol", "diesel", "electric", "hybrid", "gas"]):
            motor = part
        elif any(c in part.lower() for c in ["white", "black", "red", "blue", "gray", "green", "silver"]):
            color = part
        elif not type_:
            type_ = part

    return running, type_, transmission, wheel, motor, color

def start_driver():
    options = Options()
    options.add_argument('--headless')
    options.add_argument('--lang=en-US')
    prefs = {"intl.accept_languages": "en,en_US"}
    options.add_experimental_option("prefs", prefs)
    return webdriver.Chrome(options=options)


data = []
page = 1
max_pages = 400  
restart_interval = 50

driver = start_driver()

try:

    driver.get("https://www.auto.am/lang/en")
    time.sleep(1)  # let session set language

    while page <= max_pages:
        if page % restart_interval == 0:
            driver.quit()
            time.sleep(2)
            driver = start_driver()
            driver.get("https://www.auto.am/lang/en")
            time.sleep(1)

        url = build_url(page)
        print(f"Scraping page {page}: {url}")
        driver.get(url)
        time.sleep(3)

        soup = BeautifulSoup(driver.page_source, 'html.parser')
        ads = soup.find_all('div', id=lambda x: x and x.startswith('ad-'))

        if not ads:
            print(f"No ads on page {page}. Stopping.")
            break

        for ad in ads:
            content = ad.find('div', class_='card-content')
            action = ad.find('div', class_='card-action')
            if not content:
                continue

            lines = [line.strip() for line in content.get_text(separator="\n").split("\n") if line.strip()]
            year = lines[0] if lines else ''
            car = lines[1] if len(lines) > 1 else ''

            price = ''
            if action:
                price_tag = action.find('div', class_='price bold blue-text')
                price = price_tag.get_text(strip=True) if price_tag else ''

            details_div = ad.select_one("div.card-content div.card-desc")
            if details_div:
                details_text = details_div.get_text(separator=" ", strip=True)
                running, type_, transmission, wheel, motor, color = parse_details(details_text)

                data.append({
                    "year": year,
                    "car": car,
                    "running": running,
                    "type": type_,
                    "transmission": transmission,
                    "wheel": wheel,
                    "motor": motor,
                    "color": color,
                    "price": price
                })

        if page % 25 == 0:
            df_partial = pd.DataFrame(data)
            df_partial.to_csv("auto_am_en_data_partial.csv", index=False, encoding='utf-8-sig')
            print(f"Saved checkpoint at page {page}")

        page += 1

finally:
    driver.quit()
    df = pd.DataFrame(data)
    df.to_csv("auto_am_en_data.csv", index=False, encoding='utf-8-sig')
    print("Data saved to auto_am_en_data.csv")
