import json,os
S=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
N=['Brunca','Central Este','Central Norte','Central Occidente','Central Sur','Chorotega','Huetar Caribe','Huetar Norte','Pacífico Central']
T={17:[('Huetar Norte',761,283.25),('Pacifico Central',906,282.69),('Central Norte',1927,184.17),('Chorotega',856,181.33),('Brunca',667,176.85),('Huetar Caribe',783,163.43),('Central Este',627,109.04),('Central Sur',1357,88.00),('Central Occidente',145,None),8029],
18:[('Pacifico Central',906,282.69),('Huetar Norte',669,249.01),('Central Norte',2021,193.16),('Chorotega',865,183.23),('Brunca',673,178.44),('Huetar Caribe',732,152.79),('Central Este',665,115.65),('Central Sur',1369,88.78),('Central Occidente',151,None),8051],
19:[('Pacifico Central',925,288.62),('Huetar Norte',713,265.39),('Brunca',759,201.24),('Central Norte',2080,198.80),('Chorotega',902,191.07),('Huetar Caribe',770,160.72),('Central Este',543,94.44),('Central Sur',1415,91.76),('Central Occidente',167,None),8274],
20:[('Pacifico Central',936,292.05),('Huetar Norte',745,277.30),('Chorotega',1167,247.21),('Brunca',809,214.50),('Central Norte',2086,199.37),('Huetar Caribe',799,166.77),('Central Este',588,102.26),('Central Sur',1461,94.74),('Central Occidente',170,None),8761],
21:[('Pacifico Central',931,290.49),('Brunca',1092,289.53),('Huetar Norte',744,276.93),('Central Norte',2111,201.76),('Chorotega',808,171.16),('Huetar Caribe',819,170.95),('Central Este',743,129.22),('Central Sur',1432,92.86),('Central Occidente',171,None),8851],
22:[('Brunca',1239,328.51),('Pacifico Central',957,298.60),('Huetar Norte',794,295.54),('Central Norte',2247,214.76),('Huetar Caribe',843,175.96),('Chorotega',823,174.34),('Central Este',748,130.09),('Central Sur',1275,82.68),('Central Occidente',184,None),9110],
24:[('Brunca',1686,447.03),('Pacifico Central',1118,348.84),('Huetar Norte',864,321.59),('Central Norte',2472,236.26),('Chorotega',893,189.16),('Huetar Caribe',871,181.80),('Central Este',924,160.70),('Central Sur',1570,101.81),('Central Occidente',222,None),10620],
25:[('Brunca',1607,426.08),('Pacifico Central',1268,395.64),('Huetar Norte',921,342.81),('Central Norte',2617,250.12),('Chorotega',953,201.87),('Huetar Caribe',871,181.80),('Central Este',968,168.35),('Central Sur',1680,108.95),('Central Occidente',233,None),11118],
26:[('Brunca',2193,581.45),('Pacifico Central',1361,424.66),('Huetar Norte',959,356.95),('Central Norte',2815,269.05),('Chorotega',912,193.19),('Central Este',1065,185.22),('Huetar Caribe',886,184.93),('Central Sur',1891,122.63),('Central Occidente',246,None),12328],
27:[('Brunca',2323,615.92),('Pacifico Central',1503,468.97),('Huetar Norte',994,369.98),('Central Norte',2982,285.01),('Central Este',1124,195.48),('Chorotega',917,194.25),('Huetar Caribe',915,190.99),('Central Sur',2058,133.46),('Central Occidente',244,None),13060],
28:[('Brunca',2327,616.98),('Pacifico Central',1632,509.22),('Huetar Norte',1045,388.96),('Central Norte',3157,301.73),('Central Este',1191,207.13),('Chorotega',967,204.84),('Huetar Caribe',921,192.24),('Central Sur',2203,142.86),('Central Occidente',262,None),13705]}
T[23]=T[22]
V={}
for n,f in T.items():
    notas=[('nombre_no_reconocido','Fila "OCCIDENTAL" vacía en la imagen de la tabla: no está en la lista, no se mapea ni se incluye como fila (no tiene cifras).'),
           ('tasa_vacia_en_pdf','Central Occidente aparece sin tasa en el PDF: tasa_100k y poblacion_implicita vacías.')]
    if n==22: notas.append(('imagen_baja_calidad','Imagen borrosa; lectura confirmada por suma = total y por ser idéntica a la del N°23.'))
    if n==23: notas.append(('tabla_obsoleta','La imagen de la tabla es idéntica a la del N°22 (total 9110), pero el párrafo del N°23 dice 9912 casos a SE 23. Se registra lo que muestra la tabla.'))
    V[f'2024_semanal_{n}.pdf']=dict(pagina=4,anio_datos=2024,total=f[-1],filas=[list(x) for x in f[:-1]],notas=notas)
def info(pdf,pg,ay,vals,tot,corte,col=False):
    notas=[('infografia','Sin tabla: cifras por región en infografía, sin tasa y sin SE impresa ('+corte+'). se_corte vacío; no se dedujo.')]
    if col: notas.append(('infografia','Cifras identificadas solo por color: el mapa del propio boletín (p. 2) da la simbología color→región. Las 9 cifras coinciden con el semanal de igual total.'))
    V[pdf]=dict(pagina=pg,anio_datos=ay,total=tot,filas=[[n,v,None] for n,v in zip(N,vals)],notas=notas)
info('2023_mensual_idx135.pdf',5,2023,[222,293,2412,137,720,1027,4132,728,1856],11527,'"Casos al 30 de setiembre, 2023"',True)
info('2023_mensual_idx142.pdf',5,2023,[334,617,4110,237,1405,2398,5218,1319,2556],18194,'"Casos al 31 de octubre, 2023"',True)
info('2023_mensual_idx148.pdf',10,2023,[471,1031,5592,344,2297,3819,6055,1590,3715],24914,'"Casos al 30 de noviembre, 2023"',True)
info('2024_semanal_idx160.pdf',5,2024,[304,294,1438,108,796,691,378,458,933],5400,'"acumulados al 27 de febrero"')
info('2024_semanal_idx163.pdf',3,2024,[493,471,1640,126,1076,790,540,476,825],6437,'"acumulados al 17 de marzo del 2024"')
info('2024_semanal_idx189.pdf',7,2024,[3338,2143,4233,344,3466,1065,1021,1305,2096],19011,'"Casos al 30 de agosto"')
json.dump(V,open(S+'/datos/intermedios/visual.json','w'),ensure_ascii=False,indent=0)
