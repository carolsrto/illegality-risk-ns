import os
from datetime import datetime

from src.scraper import Scraper

if __name__ == '__main__':
    scraper = Scraper()
    start_time = datetime.now()
    print("Script started at:", start_time)
    dirname = os.path.dirname(__file__)

    input_dir = dirname + "/pdfs/raw/"
    html_dir = dirname + "/pdfs/html/"
    output_dir = dirname + "/pdfs/done/"
    error_dir = dirname + "/pdfs/error/"

    # Step 1: Convert PDFs into HTML using PDFNet package.
    # Scraper.pdfs_to_html generates the HTML versions of the PDFs.
    # Uncomment this first and run to create HTML versions.
    # ----
    # scraper.pdfs_to_html(input_dir, html_dir, error_dir)

    # Step 2: Get data from HTML using beautifulsoup.
    # Scraper.scrape... scrapes the HTML files from Step 1 and adds each desired value to the data frame.
    # ----
    # data_frame = scraper.scrape_cover_html_files(html_dir, output_dir)

    print("Script took:", datetime.now() - start_time, "to run.")
