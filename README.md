# CFA
CFA_data_GC.R: 
script realizzato per il post-processing dei dati (Abakus, conductivity e signal delays, flow rate, melting speed and ice height), da implementare successivamente per l'elaborazione delle acquisizioni con IPC-MS e ICP-OES. 

Lo script si compone di due sezioni: "IMPORT AND DATA MANAGEMENT" e "STATISTICS AND PLOTS". 
Nella prima sezione vengono importati i dati dalla cartella zippata CFA_HPLC e gestiti in modo tale da avere liste di dataframes (contenenti file relativi alle due bag analizzate in continuo). 

Nella seconda sezione si procede invece con l'analisi dei dati, divisi in sottosezioni (Abakus, conducibilità, draw wire, flowmeter).

#ABAKUS (1 acquisizione/sec)
Si elaborano i due df "BAG1_abakus" e "BAG2_abakus" ottenendo: 
-grafici a barre (Tot counts per channel)
-Distribuzioni cumulate (singole e di confronto per le due bag)
-Distribuzioni differenziali per volume (singole e di confronto per le due bag)
-Distribuzioni differenziali per conteggi (singole e di confronto per le due bag)

#CONDUCTIVITY (1 acquisizione/sec)
Si creano due df "BAG1_cond", "BAG2_cond", si eliminano gli spikes e si procede con lo smoothing dei dati per eliminare il rumore di fondo. Sui dati smoothati si individuano i picchi, le loro aree e si calcolano i ritardi di segnale registrati dai tre sensori di conducibilità (note aree e altezze, calcolo sigma come 0.3989*A/H). 
Si ottengono quindi:
-grafici (#2) per i dati grezzi di conducibilità con smoothing lines 
-grafico (solo BAG2, scelta per il miglior segnale) con smoothing lines sul dato normalizzato e aree dei picchi in evidenza

#DRAW WIRE (1 acquisizione/50 millisec)
Si creano df distinti per Ice Height (BAG1,BAG2) e Melt Speed (BAG1,BAG2). Si mediano i dati al secondo e si ottengono i seguenti grafici:

-grafici Ice Height (mm) (BAG1, BAG2)
-grafici Melt Speed (cm/min) (BAG1, BAG2)

Si crea infine una tabella riassuntiva delle velocità di fusione, con media, stdev e rsd%. 

#FLOW METER (1 acquisizione/50 millisec)
Si creano due df per i dati di flusso (BAG1, BAG2). Si mediano i dati al secondo, si plottano i grafici di flusso per ciascuna bag e si elimina il rumore di fondo smoothando i dati e plottando il risultato sui grafici precendentemente ottenuti. Si crea infine una tabella riassuntiva dei flussi, con media, stdev e rsd%. 

Si riordinano i dati in un'unica lista "SAMPLE", contenente due liste "BAG1", "BAG2", all'interno delle quali sono riportati i dataframes ottenuti. 


#app.R 

Traduzione dello script .R con shiny. Idea di base: consentire all'utente la selezione di una delle due BAG, limitando quindi la scelta successiva del dato da visualizzare. Visualizzazione di plot interattivi e tabelle (summary). WIP...
