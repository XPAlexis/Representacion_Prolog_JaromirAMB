plantas(nogal).
plantas(nuez_vomica).
plantas(ocote).
plantas(ortiga).
plantas(orégano). 
plantas(guazuma).
plantas(guayacán).
plantas(hamamelis).
plantas(helenio).
plantas(hierba_del_pollo).


%REGLAS

%Aplica que sana con la planta   --> recetar(A,S,L).
recetar( X, Y, Z) :- elimina( X,Y ), modo_aplicacion( Y,Z ).
%manda con que se toma    --> tomar(untar,X).
tomar(X,Y):-uso(Y,X).
%nombre cientifico --> 
res(X, Y ):- nombre_cientifico(X, Y).
%modousar(anemia, S,D,F).
modousar(X, Y, Z, W):- uso(X ,Y), modo_aplicacion(X, Z), elimina(W,X).
% botiquin(X).
botiquin(X):- plantas(X).
% medicinaenferm(orégano,A,S).
medicinaenferm(X, Z, Y):- medicamento(X,Z), elimina(X, Y).
% medicina(ocote,H).
medicina(X, Z):- medicamento(X,Z).
%
c(X, Y):- cura(X, Y).

%produce_medicamento(rosal, X, Y).
alivia(X, Y):- elimina(X, Y)|elimina(X,Y).





modo(interno).
modo(externo).

modo_aplicacion(anemia, interno).
modo_aplicacion(escrofulosis, interno).
modo_aplicacion(herpes, interno).
modo_aplicacion(leucorrea, externo).
modo_aplicacion(ulceras, interno).
modo_aplicacion(globulos_rojos, interno).
modo_aplicacion(pelo, externo ).
modo_aplicacion(intestinal, interno).
modo_aplicacion(amibiasis, interno).
modo_aplicacion(disentería, interno).
modo_aplicacion(congestion, interno).
modo_aplicacion(fiebre, interno).
modo_aplicacion(estomacal, interno).
modo_aplicacion(bronquitis, interno).
modo_aplicacion(reumas, interno).
modo_aplicacion(bichos_intestino, interno).
modo_aplicacion(inflamación, externo).
modo_aplicacion(pecho, externo).
modo_aplicacion(pulmones, interno).
modo_aplicacion(flemas, interno).
modo_aplicacion(purifica_sangre, interno).
modo_aplicacion(tos, interno).
modo_aplicacion(hidropesía, interno).
modo_aplicacion(hemorragias_internas, interno).



elimina(guazuma, disentexia, digestion).
elimina(guazuma, diarrea, digestion).
elimina(guazuma, inflamacion_intestinal, inflamacion).
elimina(guazuma, estreñimiento, digestion).
elimina(guayacan, tos_Tuberculos, tos).
elimina(guayacan, sifilis, ulceras ).
elimina(guayacan, reumatismo, reumas).
elimina(guayacan, reumatismo_Cronico, reumas).
elimina(hamamelis, venas_Varicosas, varicela).
elimina(hamamelis, hemorragias, hemorragias).
elimina(hamamelis, heridas, heridas).
elimina(hamamelis, hemorroides, heridas).
elimina(hamamelis, ulceras_Varicosas, ulceras).
elimina(helenio, indigestiones, indigestion).
elimina(helenio, dolor_de_Bazo, dolor).
elimina(helenio, tifo_Exantematico, dolor).
elimina(helenio, asma, respiratorio).
elimina(helenio, sarna, piel).
elimina(helenio, retencion_de_Orina, indigestion).
elimina(hierba_de_pollo, indigestiones, indigestion).

elimina(nogal, anemia, sangre).
elimina(nogal, escrofulosis, indigestion).
elimina(nogal, herpes, piel).
elimina(nogal, leucorrea, piel).
elimina(nogal, ulceras, ulceras).
elimina(nogal, globulos_rojos, sangre).
elimina(nogal, pelo, pelo).
elimina(orégano, intestinal, digestion).
elimina(orégano, amibiasis, digestion).
elimina(orégano, disentería, dolor ).
elimina(orégano, disentería, digestion).
elimina(nuez_vomica,congestion, piel).
elimina(nuez_vomica,fiebre, piel).
elimina(nuez_vomica,estomacal, digestion).
elimina(ocote, bronquitis, garganta).
elimina(ocote, reumas, reumas).
elimina(ocote,bichos_intestino ,indigestion).
elimina(ortiga, inflamación, inflamacion).
elimina(ortiga, pecho, dolor).
elimina(ortiga, pulmones, respiratorio).
elimina(ortiga, flemas, respiratorio).
elimina(ortiga, purifica_sangre, sangre).
elimina(ortiga, tos, tos).
elimina(ortiga, hidropesía, piel).
elimina(ortiga, hemorragias_internas, hemorragias).

forma(agua).
forma(te).
forma(untar).
forma(comer).

uso(anemia, te).
uso(escrofulosis, te).
uso(herpes, untar).
uso(herpes, te).
uso(leucorrea, comer).
uso(ulceras, te).
uso(ulceras, agua).
uso(globulos_rojos, te).
uso(pelo, untar ).
uso(intestinal, te).
uso(amibiasis, te).
uso(disentería, agua).
uso(congestion, te).
uso(fiebre, te).
uso(estomacal, te).
uso(bronquitis, te).
uso(reumas, te).
uso(bichos_intestino, te).
uso(inflamación, untar).
uso(pecho, untar).
uso(pulmones, te).
uso(flemas, te).
uso(purifica_sangre, te).
uso(tos, te).
uso(hidropesía, te).
uso(hemorragias_internas, te).
uso(hemorragias_internas, agua).

nombre_cientifico(nogal, junglans_regia).
nombre_cientifico(nuez_vomica, strchnos_nux_vomica).
nombre_cientifico(ocote, pinus_teocote).
nombre_cientifico(ortiga, urtica_urens).
nombre_cientifico(orégano, origamn_vulgare).

%medicamento
medicamento(nogal, nogal_negro).
medicamento(nogal, capfint).
medicamento(nogal, tintura_nogal).

medicamento(nuez_vomica, nux_vomica30C).
medicamento(nuez_vomica, super_nux).
medicamento(nuez_vomica, nux_vomica_homaccord).

medicamento(ocote, yerbatex).
medicamento(ocote, miel).
medicamento(ocote, pomada_resina).
medicamento(ocote, ivexterm).

medicamento(ortiga, arti_king).
medicamento(ortiga, arkopharma).
medicamento(ortiga, vidanat_suplemento_alimenticio).
medicamento(ortiga, ortiga_mas_ajo_rey).
medicamento(ortiga, vi_b_flex).

medicamento(orégano, aceite_oregano).
medicamento(orégano, herbal_root).
medicamento(orégano, aromavita).
medicamento(orégano, oregano_oil).
medicamento(orégano, estafilofin).



produce_medicamento(digital, digitalina, no_hay).
produce_medicamento(digital, tonico_cardiaco,no_hay).
produce_medicamento(opio, morfina, no_hay).
produce_medicamento(opio, codeina, no_hay).
produce_medicamento(ipeba, emetina, dicenteria).
produce_medicamento(ipeba, emetina, emopticis).
produce_medicamento(ipeba, emetina, epatitis_amibiana).
produce_medicamento(nuez_vomica, estrienina, no_hay).
produce_medicamento(eleboro_blanco, veratrina, hipotensor).
produce_medicamento(colchico, colquicina, para_la_gota).
produce_medicamento(velladona, atropina, oftamologia).
produce_medicamento(quina, quinina, no_hay).
produce_medicamento(cacao, teobromina, diuretica).
produce_medicamento(retama, esparteina, tonico).
produce_medicamento(coca, cocaina, no_hay).
produce_medicamento(teyote, mescalina, no_hay).
produce_medicamento(efedra, efedrina, oftaumologia).
produce_medicamento(barbasco, ormonas, no_hay).
produce_medicamento(nenufar_amarillo, lutenurina, aintibiotico).
produce_medicamento(name, viosponina, no_hay).
produce_medicamento(artemisa, tauremicina, no_hay).
produce_medicamento(semilla_del_yute, olitorisida, glucosido).
produce_medicamento(toloache, acido_licergico, lcd).
produce_medicamento(eucalipto, eucaliptol, no_hay). 
produce_medicamento(rosal, vitaminaC, no_hay).
produce_medicamento(rosal, quercitrina, no_hay).


cura(eucalipto,bronquitis).
cura(borraja,bronquitis).
cura(anacahuite,bronquitis).
cura(gordolobo,bronquitis).
cura(tilo,bronquitis).
cura(benjui,bronquitis).
cura(marrubio,bronquitis).
cura(rabano,bronquitis).
cura(gordolobo,bronconeumonia).
cura(eucalipto,bronconeumonia).
cura(ipecacuana,bronconeumonia).
cura(mostaza,bronconeumonia).
cura(ortiga,caida_del_cabello).
cura(espinosilla,caida_del_cabello).
cura(marrubio,caida_del_cabello).
cura(romero,caida_del_cabello).
cura(anis,calambres).
cura(tila,calambres).
cura(manzanilla,calambres).
cura(ajenjo,calambres).
cura('diente de leon',calculos_biliares).
cura(aceite_de_oliva,calculos_biliares).
cura(retama,calculos_biliares).
cura(cabellos_de_elote,calculos_renales).
cura(pingüica,calculos_renales).
cura(cola_de_caballo,calculos_renales).
cura(ajo,callos).
cura(cebolla,callos).
cura(hiedra,caries).
cura(cola_de_caballo,caries).
cura(ortiga,caspa).
cura(limon,caspa).
cura(romero,caspa).
cura(cuachalalate,cancer_de_utero).
cura(llanten,cancer_de_utero).
cura(siempreviva,cancer_de_utero).
cura(mastuerzo,ciatica).
cura(higuera,ciatica).
cura(sauco,ciatica).
cura(toronjil,circulacion).
cura(sanguinaria,circulacion).
cura(salvia,circulacion).
cura(hamamelis,circulacion).
cura(cola_de_caballo,cintitis).
cura(doradilla,cintitis).
cura(ajo,cintitis).
cura(cabellos_de_elote,cintitis).
cura(menta,colicos).
cura(hinojo,colicos).
cura(manzanilla,colicos).
cura(toronjil,colicos).
cura(boldo,colicos).
cura(linaza,colitis).
cura(anis,colitis).
cura(romero,colitis).
cura(cola_de_caballo,colitis).
cura(arnica,contusiones).
cura(hamamelis,contusiones).
cura(laurel,contusiones).
cura(brionia,contusiones).
cura(digital,corazon).
cura(salvia,corazon).
cura(nuez_de_kola,corazon).
cura(tejocote,corazon).
cura(achicoria,depurativos_de_la_sangre).
cura('diente de leon',depurativos_de_la_sangre).
cura(apio,depurativos_de_la_sangre).
cura(sanguinaria,depurativos_de_la_sangre).
cura(zarzaparrilla,depurativos_de_la_sangre).
cura(berro,depurativos_de_la_sangre).
cura(matarique,diabetes).
cura(tronadora,diabetes).
cura(eucalipto,diabetes).
cura(damiana,diabetes).
cura(capulin,diarrea_cronica).
cura(mezquite,diarrea_cronica).
cura(tlalchichinole,diarrea_cronica).
cura(linaza,diarrea_por_irritacion).
cura(membrillo,diarrea_por_irritacion).
cura(arroz,diarrea_por_irritacion).
cura(cebada,diarrea_por_irritacion).
cura(guayaba,diarrea_por_inflamación).
cura(albahaca,diarrea_por_inflamación).
cura(granada,diarrea_por_inflamación).
cura(manzanilla,diarrea_verdosa).
cura(simonillo,diarrea_verdosa).
cura(siempreviva,diarrea_verdosa).

% Pagina 164
cura(chaparro_amargoso,diarrea_con_sangre).
cura(muicle,diarrea_con_sangre).
cura(monacillo,diarrea_con_sangre).
cura(limon,difteria).
cura(naranja,difteria).
cura(tamarindo,disenteria).
cura(chaparro_amargoso,disenteria).
cura(ipecacuana,disenteria).
cura(cedron,disenteria).
cura(anis,dispepsia).
cura(menta,dispepsia).
cura(yerbabuena,dispepsia).
cura(diente,dispepsia).
cura(anis,dispepsia).
cura(te_limon,dispepsia).
cura(genciana,dispepsia).
cura(tabaquillo,dispepsia).
cura(ruibarbo,dispepsia).
cura(anis_estrella,dolor).
cura(valeriana,dolor).
cura(manzanilla,dolor).
cura(alcanfor,dolores_musculares).
cura(tamarindo,empacho).
cura(linaza,enteritis).
cura(cedron,enteritis).
cura(llanten,enteritis).
cura(valeriana,epilepsia).
cura(hierba_del_pollo,epistaxis).
cura(cebolla,epistaxis).
cura(perejil,epistaxis).
cura(sauco,erisipela).
cura(hiedra,erisipela).
cura(zanahoria,erisipela).
cura(borraja,escarlatina).
cura(sauco,escarlatina).
cura(cebolla,escarlatina).
cura(ajo,escorbuto).
cura(limon,escorbuto).
cura(berro,escorbuto).
cura(cebolla,escorbuto).
cura(geranio,escorbuto).
cura(ciruela,estrenimiento).
cura(linaza,estrenimiento).
cura(chia,estrenimiento).
cura(tamarindo,estrenimiento).
cura(agar_agar,estrenimiento).
cura(eucalipto,faringitis).
cura(lavanda,faringitis).
cura(anacahuite,faringitis).
cura(apio,flatulencias).
cura(tomillo,flatulencias).
cura(perejil,flatulencias).
cura(anis_estrella,flatulencias).
cura(hinojo,flatulencias).
cura(toronjil,flatulencias).
cura(romero,flatulencias).
cura(ruibarbo,flatulencias).
cura(ruda,flatulencias).
cura(menta,flatulencias).
cura(arnica,flebitis).
cura(alfalfa,flebitis).
cura(lino,flebitis).
cura(malvavisco,flebitis).
cura(romero,flebitis).
cura(quina,flebitis).
cura(genciana,flemas).
cura(oregano,flemas).
cura(fenogreco,forunculos).
cura(malvavisco,forunculos).
cura(hiedra,forunculos).
cura(manzanilla,gastralgia).
cura(anis_estrella,gastralgia).
cura(cola_de_caballo,gonorrea).
cura(doradilla,gonorrea).
cura(zarzaparrilla,gonorrea).
cura(apio,gota).
cura(cerezo,gota).
cura(limon,gota).
cura(pino,gota).
cura(alcanfor,gota).
cura(aconito,gota).
cura(belladora,gota).
cura(belenio,gota).
cura(colchico,gota).
cura(chicalote,gota).
cura(encina,grietas_del_ano).

% Pagina 165
cura(encina,grietas_del_pezon).
cura(nogal,grietas_del_pezon).
cura(milenrana,grietas_del_pezon).
cura(eucalipto,gripe).
cura(limon,gripe).
cura(quina,gripe).
cura(zarzaparrilla,gripe).
cura(calendula,gripe).
cura(hinojo,halitosis).
cura(menta,halitosis).
cura(mastuerzo,hemorragia_interna).
cura(ortiga,hemorragia_interna).
cura(rosal,hemorragia_interna).
cura(retama,hepatitis).
cura(boldo,hepatitis).
cura(alcachofa,hepatitis).
cura(prodigiosa,hepatitis).
cura(cascara_sagrada,hepatitis).
cura(helecho,hernia).
cura(ricino,hernia).
cura(tabaco,hernia).
cura(linaza,herpes).
cura(llanten,herpes).
cura(arnica,heridas).
cura(hamamelis,heridas).
cura(alcachofa,hidropesia).
cura(cardo,hidropesia).
cura(perejil,hidropesia).
cura(sauco,hidropesia).
cura(berros,hidropesia).
cura(retama,hidropesia).
cura(marrubio,congestion_higado).
cura(boldo,congestion_higado).
cura(doradilla,congestion_higado).
cura(ruibarbo,congestion_higado).
cura(manzanilla,colicos_higado).
cura(lechuga,bilis_higado).
cura(tila,bilis_higado).
cura(papaloquelite,letexicia_higado).
cura(achicoria,letexicia_higado).
cura(berros,letexicia_higado).
cura(llanton,letexicia_higado).
cura(retame,letexicia_higado).
cura(tecomasuchil,letexicia_higado).
cura(ajo,hipertension).
cura(esparrago,hipertension).
cura(alpiste,hipertension).
cura(muerdago,hipertension).
cura(miel,hipotension).
cura(nuez_de_kola,hipotension).
cura(crategus,hipotension).
cura(acedera,hipotension).
cura(anis,hipo).
cura(hinojo,hipo).
cura(tila,hipo).
cura(valeriana,hipo).
cura(azahar,histerismo).
cura(belenio,histerismo).
cura(gelsemio,histerismo).
cura(tila,histerismo).
cura(valeriana,histerismo).
cura(pasiflora,insomnio).
cura(azahar,insomnio).
cura(menta,insomnio).
cura(manzanilla,insomnio).
cura(lechuga,insomnio).
cura(tile,insomnio).
cura(genciana,atomia_intestino).
cura(melisa,atomia_intestino).
cura(yohimbo,impotencia_sexual).
cura(damiana,impotencia_sexual).
cura(nuez_vomica,impotencia_sexual).
cura(aguacate,impotencia_sexual).

% Pagina 166
cura(manzanilla,jaqueca).
cura(aconito,jaqueca).
cura(valeriana,jaqueca).
cura(tila,jaqueca).
cura(chicalote,jaqueca).
cura(hinojo,lactancia).
cura(anis,lactancia).
cura(menta,lactancia).
cura(perejil,lactancia).
cura(zanahoria,lactancia).
cura(aconito,laringitis).
cura(borraja,laringitis).
cura(cebolla,laringitis).
cura(rosa,laringitis).
cura(benjui,laringitis).
cura(encino,laringitis).
cura(encina,leurocorrea).
cura(zarzaparilla,leurocorrea).
cura(pino,leucorrea).
cura(enebro,leucorrea).
cura(genciana,leucorrea).
cura(ajenjo,lombrices).
cura(ajo,lombrices).
cura(cebolla,lombrices).
cura(brionia,lombrices).
cura(aguacate,lombrices).
cura(papaya,lombrices).
cura(avena,lumbago).
cura(cebada,lumbago).
cura(tomillo,lumbago).
cura(verbena,lumbago).
cura(fenogreco,llagas).
cura(eucalipto,llagas).
cura(llanton,llagas).
cura(sanguinaria,llagas).
cura(quina,malaria).
cura(girasol,malaria).
cura(eucalipto,malaria).
cura(cardo,malaria).
cura(azahar,menopausia).
cura(hamamelis,menopausia).
cura(tila,menopausia).
cura(quina_roja,menopausia).
cura(azafran,menstruacion_abundante).
cura(hamamelis,menstruacion_abundante).
cura(belladona,menstruacion_dolorosa).
cura(anis_estrella,menstruacion_dolorosa).
cura(ruda,menstruacion_escasa).
cura(ajenjo,menstruacion_escasa).
cura(manzanilla,menstruacion_escasa).
cura(apio,menstruacion_irregular).
cura(hispol,menstruacion_irregular).
cura(quina_amarilla,menstruacion_irregular).
cura(sabina,menstruacion_irregular).
cura(artemisa,menstruacion_irregular).
cura(hamamelis,metorragia).
cura(zoapatle,metorragia).
cura(perejil,metorragia).
cura(cuerrecilo_centena,metorragia).
cura(clavo,muelas).
cura(hiedra,muelas).
cura(ortiga,nariz).
cura(cola_de_caballo,nariz).
cura(ruda,nariz).
cura(eucalipto,nariz).
cura(anis,nauseas).
cura(ajenjo,nauseas).
cura(menta,nauseas).
cura(salvia,nauseas).
cura(manzanilla,neuralgias).
cura(menta,neuralgias).
cura(valeriana,neuralgias).
cura(boldo,neuralgias).
cura(pasiflora,neurastenia).
cura(te_negro,neurastenia).
cura(mate,neurastenia).
cura(valeriana,neurastenia).
cura(linaza,nefritis).
cura(grama,nefritis).
cura(cebada,nefritis).
cura(llanten,nefritis).
cura(doradilla,nefritis).
cura(esparrago,nefritis).
cura(ruda,nefritis).

cura(toronjil,obesidad).
cura(marrubio,obesidad).
cura(limon,obesidad).
cura(malva,obesidad).
cura(esparrago,obesidad).
cura(boldo,oidos).
cura(aceite_de_oliva,oidos).
cura(llanten,oidos).
cura(hiedra,oidos).
cura(manzanilla,ojos).
cura(limon,ojos).
cura(llanten,ojos).
cura(salvia,ojos).
cura(ruda,ojos).
cura(rosal,ojos).
cura(ajenjo,paludismo).
cura(quina,paludismo).
cura(berro,pecas).
cura(genciana,pecas).
cura(rabano,pecas).
cura(papaya,pecas).
cura(laurel,pies_dolorosos).
cura(encina,pies_dolorosos).
cura(miel,piquetes_de_abeja).
cura(perejil,piquetes_de_abeja).
cura(cebolla,piquetes_de_abeja).
cura(puerro,piquetes_de_abeja).
cura(fresno,piquetes_de_arania).
cura(ipecacuana,piquetes_de_arania).
cura(alcanfor,piquetes_de_mosco).
cura(perejil,piquetes_de_mosco).
cura(hamamelis,piquetes_de_mosco).
cura(anagalida,piquetes_de_vibora).
cura(jengibre,pleuresia).
cura(linaza,pleuresia).
cura(cardo,pleuresia).
cura(girasol,pleuresia).
cura(ipecacuana,piorrea).
cura(cola_de_caballo,prostata).
cura(eucalipto,pulmonia).
cura(ocote,pulmonia).
cura(gordolobo,pulmonia).
cura(borraja,pulmonia).
cura(sauco,pulmonia).
cura(linaza,quemaduras).
cura(cebolla,quemaduras).
cura(hiedra,quemaduras).
cura(gordolobo,quemaduras).
cura(nogal,raquitismo).
cura(ajo,reumatismo).
cura(apio,reumatismo).
cura(borraja,reumatismo).
cura(gobernadora,reumatismo).
cura(pino,reumatismo).
cura(romero,reumatismo).
cura(sanguinaria,reumatismo).
cura(marrubio,reumatismo).
cura(tabaco,reumatismo).
cura(cabellos_de_elote,riniones).
cura(cola_de_caballo,riniones).
cura(apio,riniones).
cura(eucalipto,ronquera).
cura(pino,ronquera).
cura(gordolobo,ronquera).
cura(ajo,sabaniones).
cura(cebolla,sabaniones).
cura(estafiate,mal_de_san_vito).
cura(valeriana,mal_de_san_vito).
cura(borraja,sarampion).
cura(ortiga,sarampion).
cura(sauco,sarampion).
cura(ajo,sarna).
cura(alcanflor,sarna).
cura(menta,sarna).
cura(tomillo,sarna).
cura(romero,sarna).

cura(encina,sarpuido).
cura(salvia,sarpuido).
cura(tila,sarpuido).
cura(limon,sed).
cura(tamarindo,sed).
cura(pirul,sed).
cura(semilla_de_calabaza,solitaria).
cura(granado,solitaria).
cura(coquito_de_aceite,solitaria).
cura(raiz_de_granado_agrio,solitaria).
cura(helecho_macho,solitaria).
cura(encina,sudoracion_excesiva).
cura(alcanfor,tifoidea).
cura(borraja,tifoidea).
cura(quina,tifoidea).
cura(canela,tifoidea).
cura(romero,tifoidea).
cura(salvia,tifoidea).
cura(berro,tiña).
cura(tila,tiña).
cura(tamarindo,tiña).
cura(salvia,tiña).
cura(eucalipto,tos).
cura(capulin,tos).
cura(cedron,tos).
cura(salvia,tos).
cura(malva,tos).
cura(marrubio,tos).
cura(gelsemio,tos_ferina).
cura(quina,tos_ferina).
cura(rabano,tos_ferina).
cura(violeta,tos_ferina).
cura(jugo_de_vastago_de_platano_morado,tuberculosis).
cura(mastuerzo,tuberculosis).
cura(berro,tuberculosis).
cura(ajo,tuberculosis).
cura(eucalipto,tuberculosis).
cura(pirul,tuberculosis).
cura(pino,tuberculosis).
cura(roble,tuberculosis).
cura(cuachalalate,ulcera).
cura(sanguinaria,ulcera).
cura(cola_de_caballo,ulcera).
cura(girasol,ulcera).
cura(limon,urticaria).
cura(ruibarbo,urticaria).
cura(hamamelis,varices).
cura(castaño_de_Indias,varices).
cura(llanten,varices).
cura(toronjil,varices).
cura(apio,vejiga).
cura(cipres,vejiga).
cura(cola_de_caballo,vejiga).
cura(ortiga,vejiga).
cura(malva,vejiga).
cura(leche_de_higuera,verrugas).
cura(cebolla,verrugas).
cura(nogal,verrugas).
cura(albahaca,vertigos).
cura(espino,vertigos).
cura(menta,vomitos).
cura(tila,vomitos).
cura(marrubio,vomitos).
cura(valeriana,vomitos).
cura(salvia,vomitos).
cura(cilantro,voz).
cura(ajo,voz).
cura(limon,voz).
cura(pino,voz).

%cuales son los efectos o acciones de un medicamento en especifico?




