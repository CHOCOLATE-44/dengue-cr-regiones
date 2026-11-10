"""Baja lo que necesitan los predictores: RONI de NOAA CPC, Excel ASIS 2014 y boletín N°1 de 2020. Salta lo ya descargado."""
import os,subprocess
S=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))+"/"
MS="https://www.ministeriodesalud.go.cr/index.php/biblioteca-de-archivos-left/documentos-ministerio-de-salud/"
F={"datos/fuentes/extras/RONI.ascii.txt":"https://www.cpc.ncep.noaa.gov/data/indices/RONI.ascii.txt",
   "datos/fuentes/asis/dengue_2014.xls":MS+"vigilancia-de-la-salud/analisis-de-situacion-salud/1631-situacion-dengue-costa-rica-2014/file",
   "datos/fuentes/extras/2020_semanal_1.pdf":MS+"material-informativo/material-publicado/boletines/boletines-vigilancia-vs-enfermedades-de-transmision-vectorial/boletines-epidemiologicos-2020/7641-boletin-epidemiologico-n-1-3/file"}
os.makedirs(S+"datos/fuentes/extras",exist_ok=True)
for p,u in F.items():
    if not os.path.exists(S+p): subprocess.run(['curl','-sSL','--fail','--max-time','180','-A','Mozilla/5.0','-o',S+p,u],check=True)
    print(p,os.path.getsize(S+p))
