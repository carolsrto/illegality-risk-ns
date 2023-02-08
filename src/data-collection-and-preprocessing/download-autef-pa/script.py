import argparse
import os
import time

import requests
from bs4 import BeautifulSoup

USER_AGENT = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.72 Safari/537.36"
if not os.path.exists("pdfs/"):
    os.mkdir("pdfs/")


def parseHtml(html, tag):
    idx = -1
    idx1 = -1
    idx2 = -1
    lines = html.splitlines()

    for line in lines:
        idx = idx + 1
        if idx1 == -1:
            if line.find("<" + tag) == -1:
                continue
            else:
                idx1 = idx
        else:
            if line.find("</" + tag) == -1:
                continue
            else:
                idx2 = idx + 1
                break

    if idx1 != -1 and idx2 != -1:
        text = "\n".join(lines[idx1:idx2])
        return text

    return ""


if __name__ == "__main__":
    s = requests.Session()

    parser = argparse.ArgumentParser()

    parser.add_argument("-p", "--page", help="Start page")

    args = parser.parse_args()

    if args.page:
        start_page = int(args.page)
    else:
        start_page = 0

    # Go to first page
    print("Go to index page.")
    r = s.get(
        "https://monitoramento.semas.pa.gov.br/simlam/index.aspx",
        headers={"User-Agent": USER_AGENT},
    )
    time.sleep(3)

    params = {
        "ctl00$scriptManagerMstPage": "ctl00$baseBody$upBtnLimparFiltros|ctl00$baseBody$btnBuscaAvancada",
        "__EVENTTARGET": "ctl00$baseBody$btnBuscaAvancada",
        "__EVENTARGUMENT": "",
        "__VIEWSTATE": "",
        "__VIEWSTATEGENERATOR": "",
        "__EVENTVALIDATION": "",
        "ctl00$baseBody$txtBusca": "",
        "ctl00$baseBody$txtNumeroTitulo": "",
        "ctl00$baseBody$txtNumeroProcesso": "",
        "ctl00$baseBody$txtAtividade": "",
        "ctl00$baseBody$txtNomeEmpreendimento": "",
        "ctl00$baseBody$ddlModelo": "11",  # AUTEF
        "ctl00$baseBody$txtCodigoBarras": "",
        "ctl00$baseBody$ddlMunicipio": "0",  # All Municipio
        "ctl00$baseBody$txtDataInicial": "",
        "ctl00$baseBody$txtDataFinal": "",
        "__ASYNCPOST": "true",
    }

    inputFields = ["__VIEWSTATE", "__VIEWSTATEGENERATOR", "__EVENTVALIDATION"]

    # Loop for all paginations
    page = 1
    nextPage = ""
    while True:
        # Go to 2nd page
        if params["__VIEWSTATE"] == "":
            print("Go to list page.")
            r = s.get(
                "https://monitoramento.semas.pa.gov.br/simlam/ListarLarAutef.aspx",
                headers={"User-Agent": USER_AGENT},
            )
            time.sleep(3)
        elif nextPage != "":
            print("Go to page {}".format(page))
            params["ctl00$scriptManagerMstPage"] = "ctl00$baseBody$upGrid|" + nextPage
            params["__EVENTTARGET"] = nextPage
        else:
            break

        # Search form
        print("Search form.")

        try:
            soup = BeautifulSoup(r.content, features="html.parser")
        except:
            continue

        inputs = soup.select("input")
        for input in inputs:
            try:
                name = input["name"]
                value = input["value"]
                if name in inputFields:
                    params[name] = value
            except:
                pass

        # Post params
        try:
            r = s.post(
                "https://monitoramento.semas.pa.gov.br/simlam/ListarLarAutef.aspx",
                headers={"User-Agent": USER_AGENT},
                data=params,
            )
            time.sleep(3)
        except:
            print("They're trying to hang up, hide for a bit.")
            time.sleep(30)
            r = s.post(
                "https://monitoramento.semas.pa.gov.br/simlam/ListarLarAutef.aspx",
                headers={"User-Agent": USER_AGENT},
                data=params,
            )
            time.sleep(3)

        # Update hidden fields from response
        lines = r.text.splitlines()
        for line in lines:
            if line.find("hiddenField|") != -1:
                hiddenFields = line.split("|hiddenField|")
                for hiddenField in hiddenFields:
                    parts = hiddenField.split("|")
                    name = parts[0].strip()
                    val = parts[1].strip()
                    params[name] = val
                break
        # Parse list
        html = parseHtml(r.text, "table")
        soup = BeautifulSoup(html, features="html.parser")
        trs = soup.select("tr")
        # Loop all rows excluding header and footer

        rows = []
        for tr in trs[1:-1]:
            tds = tr.select("td")

            nTitulo = tds[0].select_one("span")["title"].strip()
            nProcesso = tds[1].select_one("span")["title"].strip()
            modelo = tds[2].select_one("span")["title"].strip()
            empreendimento = tds[3].select_one("span")["title"].strip()
            municipio = tds[4].select_one("span")["title"].strip()
            _link = tds[5].select_one("a")["href"].strip()

            link = (
                _link.replace("javascript:__doPostBack('", "")
                .replace("','')", "")
                .strip()
            )

            rows.append(
                {
                    "nTitulo": nTitulo,
                    "nProcesso": nProcesso,
                    "modelo": modelo,
                    "empreendimento": empreendimento,
                    "municipio": municipio,
                    "link": link,
                }
            )

        # Get pagination row
        paginations = trs[-1].select("a")
        nextPage = ""

        if page % 10 > 0:
            for pagination in paginations:
                _link = pagination["href"]
                _page = pagination.text.strip()
                if _page == str(page + 1):
                    nextPage = (
                        _link.replace("javascript:__doPostBack('", "")
                        .replace("','')", "")
                        .strip()
                    )
                    break
        else:
            _link = paginations[-1]["href"]
            nextPage = (
                _link.replace("javascript:__doPostBack('", "")
                .replace("','')", "")
                .strip()
            )

        if page < start_page:
            print("Skip page.")

            page = page + 1

            continue

        for row in rows:
            link = row["link"]
            print("Get pdf {} - {}".format(row["nTitulo"], row["nProcesso"]))

            filename = "pdfs/{}_{}.pdf".format(row["nTitulo"], row["empreendimento"])
            if os.path.exists(filename):
                continue

            params["__EVENTTARGET"] = link
            params["ctl00$scriptManagerMstPage"] = "ctl00$baseBody$upGrid|" + link

            # Post params
            try:
                r = s.post(
                    "https://monitoramento.semas.pa.gov.br/simlam/ListarLarAutef.aspx",
                    headers={"User-Agent": USER_AGENT},
                    data=params,
                )
                time.sleep(2)
            except:
                print("They're trying to hang up, hide for a bit.")
                time.sleep(30)
                r = s.post(
                    "https://monitoramento.semas.pa.gov.br/simlam/ListarLarAutef.aspx",
                    headers={"User-Agent": USER_AGENT},
                    data=params,
                )

            # Update hidden fields from response
            codigo = ""
            lines = r.text.splitlines()
            for line in lines:
                if line.find("hiddenField|") != -1:
                    hiddenFields = line.split("|hiddenField|")
                    for hiddenField in hiddenFields:
                        parts = hiddenField.split("|")
                        name = parts[0].strip()
                        val = parts[1].strip()
                        params[name] = val

                if line.find("PaginaDownloadArquivo.aspx") != -1:
                    parts = line.split("PaginaDownloadArquivo.aspx")
                    parts = parts[1].split("'")
                    codigo = "PaginaDownloadArquivo.aspx" + parts[0]

            if codigo == "":
                print("No link exists.")
                continue

            # Download pdf
            r = s.get(
                "https://monitoramento.semas.pa.gov.br/simlam/" + codigo,
                headers={"User-Agent": USER_AGENT},
            )
            with open(filename, "wb") as f:
                f.write(r.content)

        page = page + 1
