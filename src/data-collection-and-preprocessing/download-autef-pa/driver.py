import os
import zipfile
import random
from selenium import webdriver
from selenium.webdriver.chrome.options import Options


def get_chromedriver(user_agent=None):
    path = os.path.dirname(os.path.abspath(__file__))
    chrome_options = webdriver.ChromeOptions()

    chrome_path = ""
    if os.name == "nt":
        chrome_path = "chromedriver.exe"
    else:
        chrome_path = "chromedriver"

    if user_agent:
        chrome_options.add_argument("--user-agent=%s" % user_agent)

    driver = webdriver.Chrome(
        os.path.join(path, chrome_path), chrome_options=chrome_options
    )

    return driver
