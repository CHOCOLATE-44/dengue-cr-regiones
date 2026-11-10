import json,re,os,collections,sys
S=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
m=json.load(open(S+"/datos/intermedios/manifest.json"))
NUM=r'\d[\d.,  ]*\d|\d'
ROW=re.compile(r'^\s*([A-Za-zÁÉÍÓÚáéíóúÑñüÜ][A-Za-zÁÉÍÓÚáéíóúÑñüÜ. ]*?)\s{2,}(\d.*?)\s*$|^\s*([A-Za-zÁÉÍÓÚáéíóúÑñüÜ][A-Za-zÁÉÍÓÚáéíóúÑñüÜ. ]*?)\s(\d[\d.,]*(?:\s+\d[\d.,]*)+)\s*$')
TOT=re.compile(r'^\s*(Total(?:es)?\*?|TOTAL(?:ES)?\*?|Costa Rica)\s+\d')
HDR=re.compile(r'^\s*(Regi[oó]n|Provincia)\b',re.I)
TITLE=re.compile(r'(Cuadro|Tabla|Figura|Gr[aá]fico)\s*(N[°º.]*\s*)?\d+|Costa Rica\s*:')
def parse_row(l):
    mm=ROW.match(l)
    if not mm: return None
    name=(mm.group(1) or mm.group(3)).strip(); rest=(mm.group(2) or mm.group(4))
    toks=rest.split()
    if not all(re.fullmatch(r'[\d.,]+%?',t) for t in toks): return None
    return name,toks
def title_for(pages,pi,li):
    # recorre hacia arriba desde la fila li hasta 14 lineas (y pagina previa si hace falta)
    lines=pages[pi].split('\n')[:li]
    prev=[]
    if pi>0 and sum(1 for x in lines if x.strip())<6: prev=[x for x in pages[pi-1].split('\n') if x.strip()][-8:]
    seq=[x for x in prev+lines if x.strip()][-14:]
    idx=None
    for i in range(len(seq)-1,-1,-1):
        if re.search(r'(Cuadro|Tabla)\s*(N[°º.]*\s*)?\d+',seq[i]) or re.search(r'Costa Rica\s*:',seq[i]) :
            idx=i
            if re.search(r'(Cuadro|Tabla)\s*(N[°º.]*\s*)?\d+',seq[i]): break
            if i>0 and re.search(r'(Cuadro|Tabla)\s*(N[°º.]*\s*)?\d+',seq[i-1]): idx=i-1
            break
    if idx is None: return ' '.join(' '.join(seq[-4:]).split())
    return ' '.join(' '.join(x for x in seq[idx:] if not HDR.search(x)).split())
def se_from(t):
    x=re.findall(r'(?:semana\s+epidemiol[oó]gica|semana|SE)\s*(?:N[°º.]*\s*)?(\d{1,2})\b',t,re.I)
    return [int(v) for v in x]
out=[]
for r in m:
    txt=open(S+"/datos/intermedios/txt/"+r['pdf'][:-4]+".txt").read()
    pages=txt.split('\f')
    for pi,p in enumerate(pages):
        lines=p.split('\n')
        for li,l in enumerate(lines):
            if not TOT.match(l): continue
            tr=parse_row(l.replace('*',' '))
            if not tr: continue
            rows=[];j=li-1;gap=0
            while j>=0 and gap<=2:
                if not lines[j].strip(): gap+=1;j-=1;continue
                pr=parse_row(lines[j])
                if pr is None: break
                rows.append(pr);gap=0;j-=1
            rows.reverse()
            if len(rows)<3: continue
            title=title_for(pages,pi,j+1)
            out.append(dict(pdf=r['pdf'],page=pi+1,title=title,total=tr[1],rows=rows,se=se_from(title),
                            lines_above=[x.strip() for x in lines[max(0,j-1):j+1]]))
json.dump(out,open(S+"/datos/intermedios/blocks.json",'w'),ensure_ascii=False,indent=0)
print(len(out))
c=collections.Counter()
for b in out:
    t=re.sub(r'\d+','N',b['title'])[:80]; c[(b['pdf'][:4],len(b['rows']),len(b['total']),t)]+=1
for k,v in sorted(c.items(),key=lambda kv:(kv[0][0],-kv[1])):
    if re.search('dengue|regi|provin',k[3],re.I) or v>2: print(v,k)
