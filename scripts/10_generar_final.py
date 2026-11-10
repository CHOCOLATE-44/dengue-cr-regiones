"""final.xlsx: respuesta y predictores por región y año, con su descripción al pie."""
import os
import pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Alignment,Border,Font,PatternFill,Side
S=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))+"/"
d=pd.read_csv(S+"datos/intermedios/panel_modelo.csv").sort_values(['anio','region'])
d['respuesta_nota']=None; d.loc[(d.region=='Brunca')&(d.anio==2024),'respuesta_nota']='SE40 > cierre, inflada ≥ 6,5 %'
COL=[('anio','Año','1F3864',8,'0'),('region','Región rectora','1F3864',19,None),
     ('casos','Casos de dengue SE 1–40 (respuesta)','C55A11',17,'#,##0'),('poblacion','Población','7F7F7F',13,'#,##0'),
     ('inmunidad','Inmunidad','2E75B6',12,'0.00'),('arrastre','Arrastre','548235',12,'0.00'),('oni','ONI','7030A0',12,'0.00'),('respuesta_nota','respuesta_nota','C55A11',30,None)]
SUAVE={'casos':'FBE5D6','poblacion':'EDEDED','inmunidad':'DDEBF7','arrastre':'E2EFDA','oni':'E4DFEC'}
VACIO=PatternFill('solid',fgColor='F8CBAD'); AJUSTE=PatternFill('solid',fgColor='FFE699')
borde=Side(style='thin',color='BFBFBF'); caja=Border(left=borde,right=borde,top=borde,bottom=borde)
ws=Workbook().active; wb=ws.parent; ws.title='Final'
for j,(c,t,col,w,f) in enumerate(COL,1):
    x=ws.cell(1,j,t); x.font=Font(name='Arial',bold=True,color='FFFFFF',size=11); x.fill=PatternFill('solid',fgColor=col)
    x.alignment=Alignment(horizontal='center',vertical='center',wrap_text=True); x.border=caja
    ws.column_dimensions[x.column_letter].width=w
ws.row_dimensions[1].height=48
for i,r in enumerate(d.to_dict('records'),2):
    gris=r['anio']%2==0
    for j,(c,t,col,w,f) in enumerate(COL,1):
        v=r[c]; x=ws.cell(i,j,None if pd.isna(v) else v); x.font=Font(name='Arial',size=10); x.border=caja
        if f: x.number_format=f
        if c in('anio','region'):
            x.alignment=Alignment(horizontal='center' if c=='anio' else 'left')
            if gris: x.fill=PatternFill('solid',fgColor='D9E1F2')
        elif c=='respuesta_nota':
            if not pd.isna(v): x.fill=AJUSTE
        elif pd.isna(v): x.fill=VACIO
        elif c=='casos' and not pd.isna(r['respuesta_nota']): x.fill=AJUSTE
        elif (c in('inmunidad','arrastre') and r['poblacion_fuente']=='2015_aprox') or (c=='arrastre' and r['arrastre_fuente']=='negativo_a_cero'): x.fill=AJUSTE
        else: x.fill=PatternFill('solid',fgColor=SUAVE[c])
fin=len(d)+1
ws.freeze_panes='C2'; ws.auto_filter.ref=f"A1:H{fin}"
NOTAS=[('Descripción',None,None),
 ('Casos de dengue SE 1–40 (respuesta)','Casos notificados acumulados de la semana epidemiológica 1 a la 40 en la región y el año, escalados al total nacional de la OPS.','FBE5D6'),
 ('Población','Población de la región en el año; se usa como offset. Sale de las tasas que publica el Ministerio de Salud.','EDEDED'),
 ('Inmunidad','log(tasa por 100 000 de las SE 1–40 del año anterior + 1). Efecto esperado: negativo.','DDEBF7'),
 ('Arrastre','log(tasa por 100 000 de las SE 41 al cierre del año anterior + 1). Efecto esperado: positivo.','E2EFDA'),
 ('ONI','Promedio de las temporadas JAS a NDJ del año anterior, con el RONI de NOAA CPC, índice oficial vigente. Es igual para todas las regiones en cada año. Efecto esperado: positivo.','E4DFEC'),
 ('Celdas amarillas','Valores ajustados. En 2015, inmunidad y arrastre usan casos 2014 sin escalar y la población de 2015 como aproximación. Brunca y Pacífico Central 2025 y Brunca 2026 tienen arrastre con 0 casos, porque el cierre fue menor que el acumulado a SE 40. La respuesta de Brunca 2024 no se corrigió: su acumulado a SE 40 supera al cierre.','FFE699'),
 ('Celda vacía','Occidente 2018 no tiene población publicada (0 casos). modelo_metodologia.qmd la interpola entre 2017 y 2019.','F8CBAD')]
i=fin+2
for a,b,c in NOTAS:
    if b is None:
        x=ws.cell(i,1,a); x.font=Font(name='Arial',bold=True,size=11); i+=1; continue
    ws.merge_cells(start_row=i,start_column=1,end_row=i,end_column=2); ws.merge_cells(start_row=i,start_column=3,end_row=i,end_column=8)
    x=ws.cell(i,1,a); x.font=Font(name='Arial',bold=True,size=10); x.alignment=Alignment(vertical='center',wrap_text=True)
    for j in (1,2): ws.cell(i,j).fill=PatternFill('solid',fgColor=c)
    y=ws.cell(i,3,b); y.font=Font(name='Arial',size=10); y.alignment=Alignment(vertical='center',wrap_text=True)
    ws.row_dimensions[i].height=15*(len(b)//100+1)+6; i+=1
ws.page_setup.fitToWidth=1; ws.page_setup.fitToHeight=0; ws.sheet_properties.pageSetUpPr.fitToPage=True; ws.print_title_rows='1:1'
wb.save(S+"datos/procesados/final.xlsx"); print(fin-1,'filas')
