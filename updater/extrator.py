#!/usr/bin/env python3
# encoding=utf8
from datetime import datetime
import time
import io
import requests

def extrator_web(lista_url=[]):
    ''' Extrai conteudo de cada url'''
    for url in lista_url:
        dthora = datetime.today().strftime('%Y-%m-%d-%Hh%Mmin%Ss')
        data = requests.get(url)
        if data.status_code == 200:

            nome = "focos_brasil_48h" if ("focos_brasil_48h" in url) else "focos_ams_48h"
            nome_completo = f"./data/{nome}_{dthora}.csv"

            with io.open(nome_completo, 'w') as f:
                f.writelines(data.text)

        else:
            y = data.status_code
            print("Erro conectividade!")


def ler_url():
    ''' Ler lista de url do arquivo url.txt '''
    lista_url = []
    with io.open("url.txt", mode="r", encoding="utf-8", newline='\n') as f:
        for line in f:
            lista_url.append(str(line).replace('\n',''))
    return lista_url


def main():
    ''' Programa Principal '''
    lista = ler_url()
    extrator_web(lista)


if __name__ == "__main__":
    main()
