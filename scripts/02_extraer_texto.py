import json,subprocess,os,re
from concurrent.futures import ThreadPoolExecutor
S=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
P=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))+"/datos/fuentes/pdfs/"
m=json.load(open(S+"/datos/intermedios/manifest.json"))
os.makedirs(S+"/datos/intermedios/txt",exist_ok=True)
def f(r):
    if not r['ok']: return None
    o=subprocess.run(['pdftotext','-layout',P+r['pdf'],'-'],capture_output=True)
    t=o.stdout.decode('utf8','replace')
    open(S+"/datos/intermedios/txt/"+r['pdf'][:-4]+".txt",'w').write(t)
    i=subprocess.run(['pdfinfo',P+r['pdf']],capture_output=True).stdout.decode('utf8','replace')
    n=int(re.search(r'Pages:\s+(\d+)',i).group(1))
    return r['pdf'],n,len(t),o.returncode
with ThreadPoolExecutor(8) as ex: res=[x for x in ex.map(f,m) if x]
import collections
print(len(res))
low=[x for x in res if x[2]/max(x[1],1)<300]
print('poco texto',low)
