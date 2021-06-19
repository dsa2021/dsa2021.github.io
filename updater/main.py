#!/usr/bin/env python3
import requests
import sys

# Antiga URL = "https://brasil.io/api/dataset/covid19/caso/data/?city=Manaus"
URL_UF  = "https://api.brasil.io/v1/dataset/covid19/caso_full/data/?state=CE&is_last=True&page=1"
URL_MUN = "https://api.brasil.io/v1/dataset/covid19/caso/data/?city=Fortaleza"

h=dict()
h['Authorization'] = 'Token ' + ( str(sys.argv[1]) if (len(sys.argv)>1) else '')
data = requests.get(URL_MUN, headers=h)

if data.status_code == 200:
    with open('./data/data.json', 'w') as f:
        f.write(data.text)
