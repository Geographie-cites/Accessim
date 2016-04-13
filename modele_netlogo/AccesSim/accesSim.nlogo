extensions [table array]

;; ---------------------
;; Breeds
;; ---------------------

breed [ nodes node ] ;; les noeuds du r?seau
breed [ services service ] ;; les services du modele
breed [ individuals individual ] ;; les individus
breed [ quartiers quartier] ;; spatialement, un ensemble de patches, ayant ses propres attributs (liste d'itineraires)
breed [ myservices myservice]
breed [ chemins chemin]
undirected-link-breed [arcs arc]
;; ---------------------
;; Variables
;; ---------------------

globals 
[
  loading
  simulTime ;; compteur general des tours depuis le debut de la simulation
  compteurPasTotal ; pour connaître la distance moyenne au service des individus
  max_graphe_temps
  graphe_temps
  max_graphe_temps2
  graphe_temps2
  max_graphe_richesse
  hideIndividuals
  ;;Ajout sebastien
  nb_individual_expl ;;Nombre d'individus explorateur
  nb_individual_suiveur ;; Nombre d'individus suiveur
  g_frequence_max_model;
  jeu1OK?
]

arcs-own
[
  value
  epaisseur
]

nodes-own
[
  node-id
  node_attribut
]

quartiers-own
[

  id
  txPop
  
  compteurDeparts
  compteurSatisfaits
  bonheur
  txSatisfaction
  
  ;;Ajout seb
  nb_individu_tot
  best_temps
]

patches-own 
[
  idQuartier ; valeur indiquant le quartier d'appartenance du patch
  coeffPop1
  coeffPop2 ; valeur dans [0..1]pour le choix alea au prorata de la loc de l'individu
  coordProj
  linkProj
]


chemins-own 
[
  ch_idQuart
  ch_listeChemins
  ch_listeServices
  ch_frequence
  ch_frequenceCumule
  ch_satisfaction
  ch_distance
  ch_distanceCumule
  ch_temps
  ch_tempsCumule
]

services-own 
[ 
 isMyService?
  id
  irisService ;; code de l'iris dans lequel est contenu la service.
  
  stock ;; la quantite de biens possedes par le service
  soldUnits ;; liste contenant les chiffres de vente des dernieres iterations
  achalandage ;; liste

  ;; Il faudra rajouter dans l'interface les param?params alpha et beta pour les tests de renouvellement
  
  actualLinkAcces ;; le link par lequel on accede au service
  noeudAcces1 ;; noeud par lequel on a accès à l'actualLink de la service
  noeudAcces2 ;; noeud par lequel on a accès à l'actualLink de la service
  sizeInit
  tempsDepuisOuverture
  linkService ;; "adresse" de la boulangerie
  nbPersonneFile ;; Le compteur de personne qui ont attendus, sert à donner les tickets aux personnes
  tailleFileAttente ;; 
  numeroFileIndividuSuivant ;; le numero appelé
  besoin_insatisfait ;;Quantité de personne insatisfaite entre deux recalcul de production
  argent
  has_close ;; defini letat douverture fermeture du service
]


individuals-own
[
  idQ
  besoin ;; besoin de l'individu. Augmente quand il se deplace, diminue quand il consomme des biens
         ;; pour satisfaire ses besoins
  
  noeudSuivant     ;; noeud vers lequel l'individu se deplace
  noeudActuel      ;; noeud vers lequel l'individu vient de partir ou noeud sur lequel il se situe
  noeudPrecedent   ;; noeud sur lequel l'individu est passe avant d'atteindre le noeud actuel
  
  cible             ;; objet cible de l'individu dans son deplacement 
                    ;; - soit le noeud suivant 
                    ;; - soit une service
  cibleReseau ;; lorsque l'individu cherche le réseau, au début
  chercheReseau? ;; true si pas encore sur le réseau, false si sur le réseau
  lienReseau ;; lien lorsqu'on se raccroche au réseau
  noeudReseau ;; noeud lors de l'entree dans le réseau
  cpteurLastVisite ;; nombre de tour depuis le dernier passage de l'individual dans une boulagerie
  
  actualLink ;; link sur lequel se trouve l'individu

  irisIndividual ;; iris dans lequel habite l'individual. Utilisation pour les statistiques
  patchResidence  ;; position de la maison de l'individual
  
  vitesse
  contrainteTemps ;; contrainte de temps de sortie; le compteur pas ne peut depasser 
                  ;; cette valeur au cours d'une sortie 
  satisfaction
  cheminParcouru
  ;; Memoire du chemin parcouru??? retour maison? a
  cheminFutur
  
  ;;AJOUT SEBASTIEN
  type_individual ;;Type des individus : explorateur(expl 1), suiveur(suiv 2), errant (suiv 3) indifférencié(noname 0)
  has_find_node ;; A ton atteint un noeud sur le path?
  has_find_service ;; 0 si le service n'est pas existant, 1 si service est atteint physiquement, 2 si le service est apercu
  has_statebase ;; 0 si l'on marche, 1 si l'on bloqué sur un service, 2 si l'on rentre chez nous
  has_muted ;; Est ce que l'individu a changé d'etat ? Seul moyen de le savoir, soit avec un boolean, soit en interrogeant la mémoire ... on preferera le booleen ici.
  idCheminEnCours ;; ID de l'agent chemin que l'on parcoure
  vitesseLinkActuel ;; Vitesse actuelle sur le link
  estMort ;; Variable bool?enne qui nous dit si on peut reinitialis? ou non le bonhomme
  compteurTemps ;; On compte le temps mis par chaque individu, le nombre de pas en fait 
  compteurDistance ;; On compte la distance parcouru par chaque individu (fonction vitesse et valuation de l'arc)
  idQuart ;; Who du quartier
  waitingInWhatService;; Dans quel service attend t on ? On stocke le who du service; On aurait pu le retrouver avec la position de l'individu, mais c'est moins propre
  ticketService ;; numero de ticket pour le service ou l'on est
  icolor ; la couleur de l'individu
 
  memEvtChemin ;; La mémoire de l'individu qui contient les evt chemins
]


;; ---------------------------------------------------------------
;; Procedure principale du modele; appelle toutes les autres
;; ---------------------------------------------------------------

to startup
  ca
  setup
end

to victory
ca
let temp random 280
ask patches [set pcolor random temp]
repeat random 40 [diffuse pcolor 1]
end

to go
if jeu1OK? = 0 or jeu1OK? = false [set jeu1OK? graphe_temps < Seuil_Satisfaits and graphe_temps2 < Seuil_Reelu and sum [argent] of services > Somme_par_Habitant * count individuals]
if perfs_myservice > Seuil_Entrepreneur [
victory
]



placerBoulangerie
 if isMakingMovie? [movie-grab-view]

  if labels? = false [ask turtles [ set label "" ]]
  let patchSouris 0
  set simulTime simulTime + 1
  ask services [set tempsDepuisOuverture tempsDepuisOuverture + 1]
  
  askIndividu
;  
;    if mouse-down?
;    [
;      set patchSouris patch mouse-xcor mouse-ycor
;    ]
;    
;  if mouse-down?
;  [
;    set patchSouris patch mouse-xcor mouse-ycor
;  ]
;  
  ;; ANCIEN CODE
  ;if (nbIndividuals > count individuals) 
  ;[
    ;createIndividuals (nbIndividuals - count individuals)  ;; on cree autant d'individus qu'il y a eu de morts (die) dans la journee
  ;]
  
  ;; FIXME  : Faire que ca soit les individus en rentrant chez eux qui demande leur remplacement
  ;; Permettrai de mieux controler ce qui se passe, nottament dans les files d'attentes, evt pour le moment qui n'est pas géré
  if any? individuals with [ estMort = true ]
  [
    placerBoulangerie
    ask individuals with [ estMort = true ]
    [ 
      CptIndividu_init 
    ]
  ]
  
;  ask individuals 
;  [
;    if patchResidence = 0 [initIndividual]
;  ]
placerBoulangerie
  ask-concurrent services
  [  
    
    ifelse has_close = true [
      ;On ferme le service
      CptService_closeService
    ][
    
      ;;Si le stock = 0 ou si on atteinte la fin de periode de recalcul, alors on recalcul la production
      if (periodeRecalculProd != 0 )
      [
        if (tempsDepuisOuverture mod periodeRecalculProd = 0)
        [
          cptService_recalculProd
        ]
      ]
    
      if (tailleFileAttente != 0 )
      [
        ; on lance la gestion de service
        gestionService
      ]
    ]
    
  ]

;  if mouse-down?
;  [
;    set patchSouris patch mouse-xcor mouse-ycor
;  ]
;  
;  if patchSouris != 0
;  [
;    let nodeSouris min-one-of nodes [distance patchSouris]
;    ;set patchSouris one-of patches with []
;    ;min-one-of nodes [distance myself]
;    if creer-on-fermer-off and Nombre_Services_Max > count services [if any? services with [distance nodeSouris < 10] = false [ifelse not Creer_Ma_Boulangerie [createServices 1 nodeSouris false]
;    [if count services with [isMyService? = true] = 0 [createServices 1 nodeSouris true]]
;    ]]
;  if not creer-on-fermer-off [ask services with [distance nodeSouris < 10] [die]]
;  ]
  
  if count chemins != 0 and ticks mod PeriodeRefresh = 0 [
    affichage   
  ]
  
tick  
end

to-report CptQuartier_countChemin [ objQuartier ]

 report (count chemins with [[ch_idQuart] of self = [id] of objQuartier ])

end 

to placerBoulangerie
let patchSouris 0
if mouse-down? [
   set patchSouris patch mouse-xcor mouse-ycor
]
  if patchSouris != 0
  [
 ; ask individuals [set hidden? false]

;if hideIndividuals [ask patches [set pcolor backgroundColor]]
;set hideIndividuals false
;    ;let nodeSouris min-one-of nodes [distance patchSouris]
    ;----------------------------------
    ;let nodeSouris [coordProj] of patchSouris
    let nodeSouris (projection patchSouris 2)
    ;let linkServiceTemp [] of patchSouris
    ;if not is-node? nodeSouris [set nodeSouris patch (item 0 nodeSouris) (item 1 nodeSouris)]
    ;set patchSouris one-of patches with []
    ;min-one-of nodes [distance myself]
 
 ifelse not Creer_Ma_Boulangerie
 [   if creer-on-fermer-off and Nombre_Services_Max > count services [
      if any? services with [distance nodeSouris < dist_services] = false [
          createServices 1 nodeSouris false
      ]
    ]
   ][
      if creer-on-fermer-off and any? services with [distance nodeSouris < dist_services] = false [
          if count services with [isMyService? = true] = 0 [createServices 1 nodeSouris true]
      ]
  ]
    
  if not creer-on-fermer-off [ask services with [distance nodeSouris < dist_services] [set has_close true]]
  ]

end

to CptIndividu_AccrocheReseau

  if (cible = 0)
  [
    ifelse type_individual = 2
    [
      CptIndividu_setOrientationReseauGoToNode (item 0 (cheminFutur))  
      ;show word "CptIndividu_setOrientation > GoToNode" (item 0 (cheminFutur))
    ]
    [
      CptIndividu_setOrientationReseauGoToPatch self
      ;show "CptIndividu_setOrientation > GoToPatch"
    ]
    
    ;La cible est un patch, on réoriente vers un noeud
    if is-patch? cible and chercheReseau? = false
    [
      if type_individual = 1
      [
        ;;La nouvelle cible est égale au noeud le plus proche
        CptIndividu_setOrientationReseauGoToNode noeudReseau 
        ;show word "CptIndividu_setOrientation > GoToNode"  noeudReseau 
      ]
    ]
  ]
        
  CptIndividu_avancerReseau cible
        
  set chercheReseau? CptIndividu_WatchReseau cible
  
  
 
end 

to CptIndividu_rechercheService

;  ;; Si cheminFutur = 0, que l'on a pas trouvé le service attendu en tant que suiveur, on met a jour la pénalité du chemin dans le quartier, et on devient explorateur
;  ;; FIXME : Ecriture dans une procedure a part si réutilisation multiple?
;  if (type_individual = 2 AND has_find_service = false AND length(cheminFutur)= 0 )
;  [  
;    ;;On teste si jamais le chemin a été supprimé alors que quelqu'un l'utilise
;    ifelse (chemin idCheminEnCours != nobody)
;    [
;      NewKnow
;      Show word "Suiveur > je n'ai pas trouvé le service indiqué dans mon chemin > length " length(cheminFutur)
;      
;      set type_individual 1
;      ][
;      set type_individual 1
;    ]
;  ]

  if  has_find_node = true
  [
    CptIndividu_setOrientation self
    ;show word "CptIndividu_setOrientation > Simple" cible
  ]
          
  ;set besoin besoin + parFatigue
      
  ;;On avance vers la cible
  CptIndividu_avancer cible self ;; l'individual avance dans la direction définie dans setOrientation

  ;On teste le chemin et les services
  set has_find_node (CptIndividu_watchPath self cible)
  set has_find_service (CptIndividu_watchService self)

  ;; Equivalent d'une memoire, on inscrit le chemin parcouru
  ;; Si l'on est sur un noeud ou que l'on rencontre un service, on sauvegarde l'arc entier dans tout les cas (un peu comme si l'on sauvegardé la rue entière, et pas que le carrefour)

  ;;La différence d'entregistrement est géré dans le savePathMemories
  CptIndividu_savePathMemories self cible has_find_node has_find_service 
      
  ;;On charge en mémoire un chemin si il y'en a un ... autrement dit si on est un suiveur et que l'on passe sur un noeud
  CptIndividu_loadPathMemories has_find_node
      
  ;;Si l'on est sur un service qui est disponible, alors on passe a l'etat d'attente, sinon on continue à marcher
  if has_find_service = true
  [
    set has_statebase 1
    ;show "je passe dans l'etat 1 ... "
  ]
      
end

to CptIndividu_newKnownConnaissance [cmd pChemin pcheminFutur pserviceInit]

  ;;On ajoute un chemin dans la pile de connaissance  
  let t_newKnow 0
  
  ;;On ajoute un service/chemin dans la pile de connaissance
  set t_newKnow (CptIndividu_newMsgKnownConnaissanceChemin cmd pChemin pcheminFutur pserviceInit)
  set memEvtChemin lput (t_newKnow) memEvtChemin
  
end

to CptIndividu_newShowConnaissance [pcmd pTemps pDistance pCheminParcouru pService]

  ;;On ajoute un chemin dans la pile de connaissance  
  let t_newShow 0
  
  ;;On ajoute un service/chemin dans la pile de connaissance
  set t_newShow (CptIndividu_newMsgShowConnaissanceChemin pcmd pTemps pDistance pCheminParcouru pService)
  set memEvtChemin lput (t_newShow) memEvtChemin
  
end


;; Liste la suite de comportement de chaque individu réalisé a chaque ticks
to askIndividu 

  ask-concurrent individuals
  [
    ;; correction d'un bug, mais ce n'est pas satisfaisant...
    if cheminFutur = [0] [set cheminFutur []]
    
    ;; TROIS COMPORTEMENT DE BASE POUR UN INDIVIDU : 
    ;; SOIT IL ATTEND , SOIT IL RENTRE CHEZ LUI (etat 2), SOIT IL ACCEDE AU SERVICE(etat 1), SOIT IL MARCHE (etat 0), SOIT IL ACCEDE AU RESEAU
    ;; ON RAISONNE SUR LETAT FIXE AU TOUR PRECEDENT (N - 1) POUR LE TOUR ACTUEL (N)
    ;show word "has_statebase > " has_statebase
    
     ;; On incremente le pas de vie 
     set compteurTemps (compteurTemps + 1) 
      
    ifelse (has_statebase = 0)[
     
          ;;Permet de créer d'autre comportements que AccrocheReseau du moment que la cible est un noeud pour la rechercheService
          if chercheReseau? = true 
          [
            CptIndividu_AccrocheReseau
          ]
          
          if chercheReseau? = false [       
            ;La cible est un patch, on réoriente vers un noeud
            if is-patch? cible [
              if type_individual = 1 [
                ;;La nouvelle cible est égale au noeud le plus proche
                CptIndividu_setOrientationReseauGoToNode noeudReseau 
                ;show word "CptIndividu_setOrientation > GoToNode"  noeudReseau 
              ]
            ]
            
            if is-node? cible[
              CptIndividu_RechercheService
            ]
            
          ]
          
    ][
    ifelse (has_statebase = 1)[
     
        ;show word "J'ai trouvé un service, je suis un " type_individual
        
        ;; L'individu est un suiveur
        ;show "Individu est dans le service ..."
        ;Deux méthodes au cas ou on voudrai différencier la gestion des chemins par individu
        ifelse (type_individual = 2 )[
              CptIndividu_newShowConnaissance "enterService" compteurTemps compteurDistance CheminParcouru waitingInWhatService
        ][
              if (type_individual = 1)[
                ;show word self "new show connaissance !"
                CptIndividu_newShowConnaissance "enterService" compteurTemps compteurDistance CheminParcouru waitingInWhatService 
              ]
        ]
            
        ;; On tire un ticket unique dans le service
        ;; FIXME : Possibilité de transformer ce passage en methode
        if service waitingInWhatService != nobody [
          ask service waitingInWhatService 
          [
           CptService_incrFileAttente
            ; On donne le dernier ticket a l'individu meme
            set [ticketService] of myself CptService_newTicket
          ]
        ]
        ;;On passe dans l'état attente, on ne connai pas le stock du service a lavance
        
        set has_statebase 2
        ;show word "Has_satebase > " has_statebase
    ]
    [
      ifelse has_statebase = 2 [
        
        let t_pretAConsommer false
        let t_txSatisfaction 0
        
        ;Renvoie true si on est pret a consommer, cad si le ticket correspond et que l'on va etre servi
         if service waitingInWhatService != nobody
         ;; --------------------------------------------------------------------------------------
         [  ask service waitingInWhatService [
             set t_pretAConsommer (CptService_askTraitementFileAttente ([ticketService] of myself))
           ]
         ]
         if (t_pretAConsommer = true) [
         
           ask service waitingInWhatService [
             set t_txSatisfaction ( CptService_repondBesoin myself )
             ;show word "tx_satisfaction > "t_txSatisfaction
           ]
            
            ; Si mon besoin n'est pas fini ou que je ne suis pas mort alors je reste en attente, sinon
            ;;FIXME : On peut tres bien imaginer un calcul de satisfaction qui soit impacté par l'attente
            ifelse besoin = 0 [
              ;;Creation evt Satisfaction ...
              
              ;;On indique au service que l'on part.
              cptIndividu_leaveFileAttente
                              
              ;;Retour @ home
              set has_statebase 3
            ][
            
              ;;Creation evt Satisfaction ...
              
              ;j'avertit mes collegues si le taux de satisfaction = 0
              ifelse (t_txSatisfaction = 0)
              [
                ;; FIXME : Assez moche ici, reflechir a une methode de dialogue inter individu, en pensant le probleme de l'asynchronisme des messages recu seulement au tour d'apres par une boite aux lettres ?  
                ; exemple > CptIndividu_talkToAllIndividual( "StockVide" ) 
                ; j'avertit mes collegues qui avertissent qu'ils partent de la liste d'attente
                CptIndividu_talkOtherIndividu "StockVide"
                
                ;;On indique au service que l'on part.
                ;;FIXME : risque detre fait en double si un individu sort de la simulation juste apres conso!!
                cptIndividu_leaveFileAttente
 
                ;On part
                set has_statebase  0
                set has_find_service false    
                set waitingInWhatService 0  
     
              ][
              
              ;;FIXME : faire un comportement reinit ? car comportement partagé avec closeService ici..
              ;; RQ : je continue ma route, le changement de type_individual se fait quand j'atteint le dernier noeud de mon chemin pour un suiveur...
              ;; et donc inutile d'écraser le type_individual en le mettant a 1
              
              ;;On indique au service que l'on part.
              ;;FIXME : risque detre fait en double si un individu sort de la simulation juste apres conso!!
              cptIndividu_leaveFileAttente
           
              set has_statebase  0
              set has_find_service false
              set waitingInWhatService 0
              
              ]
            ]
          
          ]
        ]
        [
          if has_statebase = 3 
          [
            
             ;;Si waitingInWhatService != 0 alors c'est que l'on a quitté la file d'attente sans etre servi !
             ;;On décremente
             ;;FIXME : regrouper dans une fonction...
             if waitingInWhatService != 0 and service waitingInWhatService != nobody [
               ask service waitingInWhatService 
               [
                 CptService_decrFileAttente
               ]
            ]
              
            ;;L'individu peut avoir muté de suiveur a explorateur 
            ;; >> on fait donc un test sur le boolean de mutation
            ;;FIXME : on aurai pu interroger la mémoire si elle a un evtKnow, mais pour le moment on utilise un boolean !
            ifelse has_muted = true
            [
              CptIndividu_GoHomeSuiveurMute self
            ][
              ifelse type_individual = 1 [
                CptIndividu_GoHomeExplorateur self
              ][
                CptIndividu_GoHomeSuiveur self            
              ]
            ]
            
            ; L'individu rentre chez lui,
            ;;On vide la mémoire de l'individu sur le tableau noir du quartier
            ask quartiers with [id = [idQ] of myself] 
            [
              CptQuartier_gestionChemin self myself
            ]
           
            set estMort true
            
        ]
      ]
    ]
    ]
 
  ;;FIXME : probleme avec les gens qui quittent les files d'attente anormalement ... on est obligé de refaire un test qui ne rentre pas en conflit avec le reste ...
  ;;On rentre chez nous si le temps imparti est fini
  if obs_testLifeIndividual = true
  [  
    ;;Si on est dans l'etat 2 au moment de quitter la file d'attente
    if (has_statebase = 2)
    [
      cptIndividu_leaveFileAttente 
    ]
    
      set has_statebase 3
  ]
  
  ];; fin ask individuals
  

end 

to-report CptService_newTicket 

report nbPersonneFile

end 

to-report obs_testLifeIndividual
  
  if (compteurTemps > 5 * Temps_Parents); 5 ticks = 1 minute (temps réel / temps simulé)
    [
      report true
    ]
    
    report false
end

to CptIndividu_talkOtherIndividu [cmd]

  if cmd = "StockVide" [
  
  ;;Uniquement les autres tortues
   ask other individuals with [[waitingInWhatService] of self = ([who] of (service [waitingInWhatService] of myself)) and has_statebase = 2 ] 
   [
     ;show word "Donc je m'en vais ..." [who] of self 
     ;;Creation evt Satisfaction ...
     set has_statebase  0
     set has_find_service false
                  
     ;;On indique au service que l'on part.
     ;;FIXME : faire une methode , bcp plus propre... 
     ask service waitingInWhatService 
     [
       ;show "file d'attente - 1"
       CptService_decrFileAttente
     ]
    
     set waitingInWhatService 0             
    ]
  ]
end

to CptService_incrFileAttente

  ; La file d'attente est incrémenté de 1
  ;show "file d'attente + 1"
  set tailleFileAttente (tailleFileAttente + 1)
  ; Le nb est incrémenté de 1
  set nbPersonneFile (nbPersonneFile  + 1 )
  
end

to CptService_decrFileAttente

  ; La file d'attente est incrémenté de 1
  ;show "file d'attente + 1"
  set tailleFileAttente (tailleFileAttente - 1)
  
end

to-report perfs_myservice
ifelse sum [argent] of services > 0 [
report 100 * count services * sum [argent] of services with [isMyService? = true] /  sum [argent] of services
][
report 0]
end

to cptIndividu_leaveFileAttente 

    ; On quitte la file d'attente mecontent de ne pas avoir ete servi
    ; Donc nouvel evt satisfaction a rajouter ici
    ifelse waitingInWhatService != 0 and service waitingInWhatService != nobody [
    
      ask service waitingInWhatService 
      [
        CptService_decrFileAttente
      ]
    ][
      ;show " service inexistant ..."
    ]
    
    ;;EVT SATISFACTION A RAJOUTER
end

to CptIndividu_GoHomeExplorateur [pIndividu]

end

to CptIndividu_GoHomeSuiveurMute [pIndividu]

  ;show "GO SUIVEUR MUTER"
  CptIndividu_GoHomeSuiveur pIndividu

end

to CptIndividu_GoHomeSuiveur [pIndividu]

  let t_individu pIndividu
  let t_typeIndividu [type_individual] of t_individu
  let t_service_exist false
  
    ;On interroge sa mémoire pour savoir si on a trouvé le service correspondant au chemin initial ... utile car l'individu peut avoir changé d'état en cours de chemin
    ;show word "chemin Initial" CptIndividu_AskMemories t_individu "evtKnow" "get:WhoCheminInit"
    let t_cheminInitial item 0 (CptIndividu_AskMemories t_individu "evtKnow" "get:WhoCheminInit")
    let t_listeService (CptIndividu_AskMemories t_individu "evtShow" "List:AllService")
    
    ;Si le chemin existe encore ..
    if chemin t_cheminInitial != nobody 
    [
      foreach (t_listeService)
      [
        ;show word " ? = " ?
        ;show word "service chemin initial" [ch_listeServices] of chemin t_cheminInitial
        ;Si on trouve un service qui correspond a la liste mémoire ...
        if (? = [ch_listeServices] of chemin t_cheminInitial)
        [
          ;show word "Le service  A ETE TROUVE ... " ?
          set t_service_exist true
        ]
      ]
    ]
    
    if t_service_exist = false [
    ;show "Le service n'existe pas dans la mémoire"
    ;;On initialise la pile de connaissance avec un premier élément : le chemin à suivre...
    CptIndividu_newKnownConnaissance "NobodyTargetChemin" t_cheminInitial cheminFutur nobody
    ]
  
end

; Quand l'individu émet une satisfaction positive ou négative vis à vis d'un chemin
; > SatisfaitService, InsatisfaitService
to-report CptIndividu_newEvtSatisfaction [cmd idChemin valeur]
  report (list "evtSatisfaction" type_individual cmd idChemin valeur)
end

; Quand l'individu rencontre un service, il teste et log le resultat pour que le quartier agisse en consequence
to-report CptIndividu_newMsgShowConnaissanceChemin [ pcmd pTemps pDistance pCheminParcouru pService]
   report (list "evtShow" pcmd type_individual pTemps pDistance pCheminParcouru pService)
end

to-report CptIndividu_newMsgKnownConnaissanceChemin [pcmd pidChemin pchemin pservice]
  report (list "evtKnow" pcmd type_individual pidChemin pchemin pservice)
end


to CptQuartier_knowNoChemin [pchemin]

  ;; Ajout Evt satisfaction 
  
  ;;Si le chemin existe encore, on décremente
  ;show pchemin
  ifelse pchemin != 0 and chemin pchemin != nobody [
    ask chemin pchemin [ cptChemin_decrFreq self ]
  ][
  ;show "le chemin n'existe plus au moment de DECR"
  ]

end

to CptQuartier_showCheminExplorateur [pquartier pchemin  ptemps pdist pservice]
  
  let t_chemin pchemin
  let t_temps ptemps
  let t_dist pdist
  let t_service pservice
  let t_quartier pQuartier ;; Probleme ici
  let t_idQuartier [id] of t_quartier
  let t_objChemin nobody 
            
  ; On teste si le chemin existe
  set t_objChemin (CptQuartier_searchConnaissanceExistant pquartier pchemin pservice)

  ;show  "----- gestionCheminExplorateur -----"
  ;show word "-- test objChemin >" service
  
  ; Le chemin est connu du quartier
  ifelse t_objChemin != nobody 
  [
    ;On incrémente la fréquence du chemin
    ask t_objChemin [ cptChemin_incrFreq self ]
    
    ;Si la distance ou le temps est différente du chemin initial alors on fait une mise à jour
    if [ch_distance] of t_objChemin != t_dist
    [
       ask t_objChemin [ cptChemin_majDistance self t_dist ]
    ]
    
    if [ch_temps] of t_objChemin != t_temps
    [
       ask t_objChemin [ cptChemin_majTemps self t_temps ]
    ]
    
    ;show  "-- test memEvt > showOldService"
  ][; Le chemin n'est pas connu du quartier, on le créé
  
    ;;Création du chemin, initialisation des parametres, frequence initialisé à 1
    let t_idChemin (cptQuartier_createChemin t_idQuartier t_chemin 1 t_temps t_dist t_service)
    
  ]
                
end 

to CptQuartier_showCheminSuiveur [pIndividu pquartier pchemin ptemps pdist pservice]

  ;;Remarque : Comme l'individu a pu passer de l'etat suiveur à l'etat explorateur, on peut ne plus accéder au chemin initialement prévu; on interroge donc la mémoire de l'individu
  let t_individu pIndividu 
  let t_cheminInitial item 0 (CptIndividu_AskMemories t_individu "evtKnow" "get:WhoCheminInit")
    
  ;; Passage des variables à des variables temporaires
  let t_chemin pchemin
  let t_temps ptemps
  let t_dist pdist
  let t_service pservice
  let t_quartier pQuartier
  let t_idQuartier [id] of t_quartier
  let t_objChemin nobody 
  
  ;;On teste si jamais le chemin a été supprimé alors que quelqu'un d'autre l'utilise
  ;ifelse (chemin idCheminEnCours != nobody)
  ;[
    ;show  "----- gestionCheminSuiveur -----"
    ;show word "-- GestionCheminSuiveur > Service " service
    
    ; On teste si le chemin existe dans tout les cas, plus simple que de faire fonction de l'id chemin du suiveur.
    ;; FIXME on pourrait peut etre faire en sorte que le chemin ne soit pas testé si jamais la liste noeud correspond au chemin id suiveur ! 
    ;; On gagnerai en rapidité.
    set t_objChemin (CptQuartier_searchConnaissanceExistant pquartier pchemin pservice)
 
  ;; Si il y'a un service + chemin qui existe 
  ifelse t_objChemin != nobody [
  
    ;Si ce n'est pas le service + chemin initialement prévu, alors on décrémente l'ancien chemin et on incrémente celui ci
    ifelse (t_cheminInitial != [who] of t_objChemin)
    [
      ;show word "cheminInitial > " t_cheminInitial
      ;Le chemin peut avoir disparu entre temps
      if (chemin t_cheminInitial != nobody) 
      [
        ;;Remarque : La décrémentation du chemin initial peut se discuter, il existe un tas de cas particulier qui gravite autour de cette hypothèse ...
        ;; ex : Je rentre dans un service qui n'est pas le service attendu, je decremente, puis je ressort et tombe sur le service attendu, je reincrémente ... un peu absurde non?
        ask chemin t_cheminInitial [ cptChemin_decrFreq self]
      ]
      
      ask t_objChemin [cptChemin_incrFreq self]
      
      ;Si la distance ou le temps est différente du chemin initial alors on fait une mise à jour
      if [ch_distance] of t_objChemin != t_dist
      [
       ask t_objChemin [ cptChemin_majDistance self t_dist ]
      ]
    
      if [ch_temps] of t_objChemin != t_temps
      [
       ask t_objChemin [ cptChemin_majTemps self t_temps ]
      ]
    
    
    ]
    [;Si le chemin correspond au chemin initialisé, on incrémente la fréquence de ce chemin
     ; Remarque : on peut être passé par plusieurs services, ce qui fait que finalement le chemin+service peut être atteint ..
     
      ask t_objChemin [cptChemin_incrFreq self]
      ;show  "augmenter frequence suiveur"
      ;Si la distance ou le temps est différente du chemin initial alors on fait une mise à jour
      if [ch_distance] of t_objChemin != t_dist
      [
       ask t_objChemin [ cptChemin_majDistance self t_dist ]
      ]
    
      if [ch_temps] of t_objChemin != t_temps
      [
       ask t_objChemin [ cptChemin_majTemps self t_temps ]
      ]
      
    ]
  ][  ;; Si il n'y aucune correspondance entre le chemin + service parcouru et le chemin et service trouvé
  
      ;;On décrémente le chemin initial
      ;Le chemin peut avoir disparu entre temps
      if (chemin t_cheminInitial != nobody) 
      [
        ask chemin t_cheminInitial [ cptChemin_decrFreq self]
      ]
      
      ;;Création d'un nouveau chemin, initialisation des parametres
      let t_idChemin cptQuartier_createChemin t_idQuartier t_chemin 1 t_temps t_dist t_service
      
  ]
  
end

to cptChemin_decrFreq [chemin]

      ifelse [ch_frequence] of chemin >= 1 + pondereInfoServiceVide
      [
        set [ch_frequence] of chemin [ch_frequence] of chemin - pondereInfoServiceVide
        ;show word "Je décremente le chemin > " chemin
      ][
        ;show word "Je suis oublié > " chemin
        die
      ]

end

to cptChemin_incrFreq [chemin]

      ;show word "J'incremente le chemin > " chemin
      set [ch_frequence] of chemin [ch_frequence] of chemin + 1
      set [ch_frequenceCumule] of chemin [ch_frequenceCumule] of chemin + 1
end

to cptChemin_majDistance [chemin valeur]
  
  let t_valeur_moy 0
   
  ;On calcule la nouvelle moyenne de distance
  ifelse [ch_frequenceCumule] of chemin != 0
  [
    set [ch_distanceCumule] of chemin [ch_distanceCumule] of chemin + valeur
    set t_valeur_moy ([ch_distanceCumule] of chemin / [ch_frequenceCumule] of chemin)
  ][
    set t_valeur_moy valeur
  ]
  
  set [ch_distance] of chemin t_valeur_moy
  
end

to cptChemin_majTemps [chemin valeur]
  
  let t_valeur_moy 0
  
  ;On calcule la nouvelle moyenne de temps, sauf quand on a frequence cumulue = 0
  ifelse [ch_frequenceCumule] of chemin != 0
  [
    set [ch_tempsCumule] of chemin [ch_tempsCumule] of chemin + valeur
    set t_valeur_moy ([ch_tempsCumule] of chemin / [ch_frequenceCumule] of chemin)
  ][
    set t_valeur_moy valeur
  ]

  set [ch_temps] of chemin t_valeur_moy
  
end

to-report cptQuartier_createChemin [pIdQuartier pchemin pfreq ptemps pdist pservice]

 let idNewAgentChemin 0
 let t_chemin pchemin
 let t_freq pfreq
 let t_temps ptemps
 let t_dist pdist
 let t_service pservice
 let t_idQuart pIdQuartier
 
  ;; Creation chemin par hatch
  hatch-chemins 1
    [        
        set ch_idQuart t_idQuart
        set ch_listeChemins t_chemin
        set idNewAgentChemin [who] of self
        set ch_frequence t_freq
        set ch_frequenceCumule ch_frequence
        set ch_satisfaction 0
        set ch_temps t_temps
        set ch_tempsCumule ch_temps
        set ch_distance t_dist
        set ch_distanceCumule ch_distance

        set ch_listeServices t_service
    ]

report idNewAgentChemin

end

to-report CptQuartier_listCheminQuartier [quartier]
  
  ;show word "listCheminQuartier" chemins with [ [id] of quartier = [ch_idQuart] of self ]
  report ( chemins with [ [id] of quartier = [ch_idQuart] of self ])
  
end

to-report CptQuartier_searchConnaissanceExistant [quartier pCheminToFind pService]

let i 0
let objChemin nobody
let t_cheminToFind pCheminToFind

let agentSetCheminQuartier CptQuartier_listCheminQuartier quartier

  without-interruption 
  [
      ask agentSetCheminQuartier
      [
        if ([ch_listeChemins] of self = pCheminToFind)
        [
          ;show   " > chemin existe"
          ;show word " > test service correspond ? > " pService
          if [ch_listeServices] of self = pService
          [
            ;show " > service + chemin existe"
            set objChemin self
          ]
        ]
      ]
  ]
  
  report objChemin
  
end 


to-report obs_CheminMaxFreq
  report item 0([who] of max-n-of 1 chemins [ch_frequence])
end

to-report CptQuartier_cheminMaxFreq [quartier]

  ;; Je renvoi le chemin avec la frequence minimum
  report item 0 ([who] of max-n-of 1 (CptQuartier_listCheminQuartier quartier) [ch_frequence])

end

to-report CptQuartier_cheminMinTemps [quartier]
  ;; Je renvoi le chemin avec la frequence minimum
  ;show word "MIN TEMPS" item 0 ([who] of min-n-of 1 (CptQuartier_listCheminQuartier quartier) [ch_temps])
  report item 0 ([who] of min-n-of 1 (CptQuartier_listCheminQuartier quartier) [ch_temps])
end

to-report CptQuartier_cheminMinDistance [quartier]
  ;; Je renvoi le chemin avec la frequence minimum
  report item 0  ([who] of min-n-of 1 (CptQuartier_listCheminQuartier quartier) [ch_distance])
end

to CptQuartier_gestionChemin [quartier idIndividual]

  let idNewAgentChemin 0
  let frequenceInit 1
  
  without-interruption
  [
    
  if length([memEvtChemin] of idIndividual) > 0
  [

    foreach ([memEvtChemin] of idIndividual) 
    [
      
      ;;----------------------------------
      ;;----------- EVT SHOW -------------
      ;;----------------------------------
      
      let typeEvt  item 0 (?)
      if typeEvt = "evtShow"
      [
        let type_ind item 2 (?)
        let cmd item 1 (?)
        let cptTemps item 3 (?)
        let cptDistance item 4 (?)
        let idChemin item 5 (?)
        let service item 6 (?)
        
            ;show "> --------------- <"
            ;show word "> idIndividual " idIndividual
            ;show word "> type ind = 1 , cmd = " cmd
            if cmd = "enterService"
            [
              ;print ( word idIndividual "> enter service > "  ?)
              ;FIXME : le mieux serait de créer le chemin au dernier moment ... avec les bon parametres...
              ;On ajoute le chemin au quartier, car pour le moment il est indiférencié
              ;;Le chemin est init a zero, on incremente la frequence
              
              ifelse type_ind = 1
              [
                cptQuartier_showCheminExplorateur quartier idChemin cptTemps cptDistance service
              ]
              [
                cptQuartier_showCheminSuiveur idIndividual quartier idChemin cptTemps cptDistance service
              ]
            ]
      ]
      
      ;;----------------------------------
      ;;----------- EVT KNOW -------------
      ;;----------------------------------
     
      if typeEvt = "evtKnow"
      [
        let type_ind item 2 (?)
        let cmd item 1 (?)
        let idChemin item 3 (?)
        let chFutur item 4 (?)
        let service item 5 (?)
        
        if cmd = "InitTargetChemin"
        []
         
        if cmd = "NobodyTargetChemin"
        [
          ;print ( word idIndividual "> NO NO NO NO service > "  ?)
          CptQuartier_knowNoChemin idChemin
        ]
      ]
     
   ]
  ]
 ]
      
end 

to obs_showFrequenceChemin

set g_frequence_max_model [ch_frequence] of chemin obs_CheminMaxFreq
;show word "max_frequence" g_frequence_max_model
  
  ask arcs 
  [
    set epaisseur 0
  ]
  
  ask chemins
  [
    
    if length(ch_listeChemins) > 0 and ch_frequence > 0
    [
      let i 0

      let sortie length(ch_listeChemins)
 
      while [i < sortie - 1]
      [
      if is-node? (item (i) (ch_listeChemins)) and is-node? (item (i + 1 ) (ch_listeChemins))
      [
            if is-arc? arc [who] of (item (i) (ch_listeChemins)) [who] of (item (i + 1 ) (ch_listeChemins)) [
            ask arc [who] of (item (i) (ch_listeChemins)) [who] of (item (i + 1 ) (ch_listeChemins))
            [
                ;; Si le thickness est inférieur au thickness nouveau
                ;if thickness < (([ch_frequence] of myself / g_frequence_max_model) * 5)
                ;[
                set epaisseur epaisseur + [ch_frequence] of myself
                ;]
            ]
        ]
        
        ]
      set i i + 1
      ]
    ]
  
  ]
  
    set g_frequence_max_model max [epaisseur] of arcs
    
    ask arcs 
  [
    set thickness 1 + 5 * epaisseur / g_frequence_max_model
  ]



end

to MajCheminQuartier [idQuartiers idIndividual]
  
    ;le bonheur par quartier est défini ci-dessous comme le bonheur moyen des individus qui y sont nés, depuis le début
    ;set bonheur (compteurDeparts * bonheur + [satisfaction] of idIndividual) / (compteurDeparts + 1) ; update le bonheur du quartier en faisant la moyenne
    ; le bonheur par quartier est défini ci-dessous comme le bonheur du dernier individu qui update le quartier
    ;if [satisfaction] of idIndividual > 0 [set bonheur [satisfaction] of idIndividual]
    ; Update bonheur du quartier (inverse de la distance parcourue par les gens qui trouvent)
      if [satisfaction] of idIndividual > 0 
    [
        ifelse compteurSatisfaits = 0 [set bonheur [satisfaction] of idIndividual]
        [
        set bonheur ((compteurSatisfaits * compteurSatisfaits / ((compteurSatisfaits + 2)*(compteurSatisfaits + 2))) * bonheur + 4 * [satisfaction] of idIndividual * (compteurSatisfaits + 1)/ ((compteurSatisfaits + 2) * (compteurSatisfaits + 2)))
        ]
    ]
    
    ; update txsatisfaction du quartier (nombre de gens trouvant)
    ifelse compteurDeparts = 0 
    [
      if [satisfaction] of idIndividual > 0 
      [
        set txSatisfaction 1
      ]
    ]
    [
      ifelse [satisfaction] of idIndividual > 0 
      [
        set txSatisfaction ((compteurDeparts * compteurDeparts / ((compteurDeparts + 2)*(compteurDeparts + 2))) * txSatisfaction + 4 * (compteurDeparts + 1)/ ((compteurDeparts + 2) * (compteurDeparts + 2)))
      ]
      [
        set txSatisfaction (compteurDeparts * compteurDeparts / ((compteurDeparts + 2)*(compteurDeparts + 2))) * txSatisfaction
      ]
    ]
    
      set compteurDeparts compteurDeparts + 1
      if [satisfaction] of idIndividual > 0 
      [
        set compteurSatisfaits compteurSatisfaits + 1
      ]
 
    if [satisfaction] of idIndividual > 0 
    []
    
    let temp 0
    let temp2 [pcolor] of one-of patches with [idQuartier = [idQ] of idIndividual]
    ifelse max [bonheur] of quartiers > 0 [
    ;set temp backgroundColor - 7 * bonheur / max [bonheur] of quartiers ;bonheur défini de facon relative aux autres
       ;set temp backgroundColor - 7 * count / length listeChemins
   ;set temp backgroundColor - 7 * compteurSatisfaits / compteurDeparts
   ifelse satisfaction-on-distance-off
     [set temp backgroundColor - 7 * txSatisfaction]
   [ifelse bonheur > 0 [
   set temp (backgroundColor - 67 +  1 / (Temps_Parents * bonheur))] ;1 / (Seuil_Satisfaits * bonheur)) 50 * Temps_Parents]
   [set temp backgroundColor - 60]]
;   ifelse 60 * bonheur > 7 [
;   set temp backgroundColor - 7
;    ][
;    set temp backgroundColor - 60 * bonheur ; bonheur défini de facon absolue
;
;    ]
    ]
    [set temp temp2]
    
    ;if abs (temp - temp2) > 1 [
    if true [
    ask patches with [idQuartier = [idQ] of idIndividual]
      [
        set pcolor temp
      ]
    ]
  
end


;- - - - - -

to init

  set simulTime 0
  set compteurPasTotal 0
  set  nb_individual_expl 0
  set  nb_individual_suiveur 0
  
  ask services 
  [
    set stock stockInitial
    set argent stock * prix_unite
    set soldUnits 0
    if labels? [set label floor stock]
    set achalandage []
    set isMyService? false
    set nbPersonneFile 0
    set tailleFileAttente 0
    set linkService projection patch-here 3
    let temp projLink self linkService 1
    ;set loading loading + 1
  ]
  
  ask individuals [
    CptIndividu_init
    ;set loading loading + 1
    set compteurTemps floor random (5 * Temps_Parents)
  ]
;  ask services [
;    set actualLinkAcces ""
;    set noeudAcces1 ""
;    set noeudAcces2 ""
;  ]
  
end
 
;------
;------
;------

to createServices [Nb point isMine?]

      create-services Nb 
    [
      ;; pour chaque ligne, on extrait 4 informations : identifiant du service, coord en X et coord en Y
      let temp 0
      ifelse point = 0 [set temp one-of patches with [idQuartier >= 0]
       ] [
       set temp point
       ]
            set xcor [pxcor] of temp
      set ycor [pycor] of temp
      set linkService projection temp 3
      set id count services + 1

      set stock stockInitial
      set soldUnits 0
      if labels? [set label floor argent]
      set achalandage []
      set has_close false 
      set isMyService? isMine?
      set argent argent_initial    
    
      file-open fichier_format
  while [not file-at-end?]
    ;; lecture des lignes du fichier et création du noeud
 [   let items read-from-string (word "[" file-read-line "]")

    set color item 3 items
    set shape item 4 items
    set sizeInit item 5 items    

 ]
file-close
    ;set size sizeInit * (stock / max [stock] of services)
    ;set size stock
    ;set size 3 * sizeInit * (1 - exp (- stock / sizeInit)) / 2 + sizeInit / 2
    updateSizeServices
    if isMine? [set color color + 30]
    ]
    
    
end

to setup
set jeu1OK? false
  if config [import-forme]
  import-iris
  setPopToPatches
  
  import-reseau
  import-service
  createQuartiers
 ; set hideIndividuals true
  set hideIndividuals false
  ;loadProjPatches
  createIndividuals nbIndividuals
  init
  formatage_graphe
  showDensity
end
 
;; ---------------------
;; Procedures individuals
;; ---------------------

to createIndividuals [nb]  ;; création des individuals
  
  create-individuals nb
  
end

to initIndividualExplorateur 

      set type_individual 1 ;; l'individu est un explorateur
      set nb_individual_expl nb_individual_expl + 1 ;; On comptabilise en global

end

to initIndividualSuiveur

      set type_individual 2 ;; l'individu est un suiveur
      set nb_individual_suiveur nb_individual_suiveur + 1 ;; On comptabilise en global
      let t_serviceInit 0 
      
      ;; Je recupere un chemin dans le quartier origine.Les chemins sont deja trie
         ask quartier idQuart
        [
          
          ;; Regle 50 / 50 % de choix entre frequence et distance parcouru
          ;            ifelse (choixCible < gl_proportion_freq_dist)
          ;            [
          ;              set [idCheminEnCours] of myself (item 1 (item 0 l_indiceTemps))
          ;              set [cheminFutur] of myself ([listeChemins] of chemin [idCheminEnCours] of myself)
          ;              ;show word "cheminFutur > " [cheminFutur] of myself
          ;            ][
          ;              set [idCheminEnCours] of myself (item 1 (item 0 l_indiceFrequence))
          ;              set [cheminFutur] of myself ([listeChemins] of chemin [idCheminEnCours] of myself)
          ;            ]

          ;; Regle ratio d> alpha * %n
          ;; Soit p le chemin avec frequence la plus élevé, e le chemin avec le temps le plus court (efficace)
          ;; Soit d le ratio dp/de et n le ration np/ne
          
          let dp_idChemin 0
          let ne_idChemin 0
          let dp 0
          let de 0
          
          let np 0
          let ne 0
          
          let d 0
          let n 0
          
          ; Je recupere le min de indiceTemps, et le max de indiceFrequence dans la liste des chemins qui va bien
          ;show "je recupere minTemps et maxFreq "
          set dp_idChemin chemin (cptQuartier_cheminMinTemps self)
          set ne_idChemin chemin (cptQuartier_cheminMaxFreq self)
          ;show word "> 1 > " dp_idChemin 
          ;show word "> 2 > " ne_idChemin
          set dp ([ch_temps] of dp_idChemin)
          ;show word "dp >  " dp
          set ne ([ch_frequence] of ne_idChemin)
          ;show word "ne >  " ne
          
            ;;On cherche np
            set np [ch_frequence] of dp_idChemin
            
            ;;On cherche de
            set de [ch_temps] of ne_idChemin
 
            
         ;;Calcul d et n
         set d (dp / de)
         set n (np / ne)
          
          ifelse (d >= gl_proportion_freq_dist * n)
            [
              set [idCheminEnCours] of myself ([who] of dp_idChemin)
              set [cheminFutur] of myself ([ch_listeChemins] of dp_idChemin)
              set t_serviceInit [ch_listeServices] of dp_idChemin
               ;show word "je prend le chemin le plus efficace " [cheminFutur] of myself
               ;show word " val d = " d 
               ;show word " val n = " (gl_proportion_freq_dist * n )
            ][
              set [idCheminEnCours] of myself ([who] of ne_idChemin)
              set [cheminFutur] of myself ([ch_listeChemins] of ne_idChemin)
              set t_serviceInit [ch_listeServices] of ne_idChemin
              ;show word "je prend le chemin le plus fréquenté " [cheminFutur] of myself
               ;show word " val d = " d 
               ;show word " val n = " (gl_proportion_freq_dist * n )
            ]
            
          ]

  ;;On initialise la pile de connaissance avec un premier élément : le chemin à suivre...
  CptIndividu_newKnownConnaissance "InitTargetChemin" idCheminEnCours cheminFutur t_serviceInit
  
end 

to cptQuartiers_update_bestchemin
ask quartiers [
   if cptQuartier_countChemin self != 0 [
     set best_temps [ch_temps] of chemin cptQuartier_cheminMinTemps self
   ]
   
   ;;ICI qu'on implémente le best distance, etc.
]
end

to initIndividualNoeudExplorateur
;; le code suivant permet de réaliser l'accroche réseau en projection sur le réseau
;; pour diverses raisons, ce code n'est pas exploité pour l'instant
;    ifelse is-node? [coordProj] of patchResidence 
;    [
;        ask [coordProj] of patchResidence [
;         
;         set [cibleReseau] of myself patch-here
;        
;        ]
;    ][
;      set cibleReseau [coordProj] of patchResidence
;      ;set cibleReseau min-one-of nodes [distance myself]
;    ]
;    
;
;    set lienReseau  [linkProj] of patchResidence
;
;    
;      let dist1 0
;      ask [end1] of lienReseau [
;          set dist1 distance [cibleReseau] of myself
;        ]
;       
;      let dist2 0
;      ask [end2] of lienReseau [
;          set dist2 distance [cibleReseau] of myself
;        ]
;
;      ifelse dist2 < dist1 
;      [
;        set noeudReseau [end2] of lienReseau
;      ][
;        set noeudReseau [end1] of lienReseau 
;      ]



  set NoeudReseau min-one-of nodes [distance myself]
  let ListeNoeud []
  ask NoeudReseau
    [
      ;;On recupere les noeud alentours au noeudActuel
      set ListeNoeud link-neighbors
  ]
  let temp one-of ListeNoeud
  set lienReseau arc [who] of NoeudReseau [who] of temp
 ; set cibleReseau NoeudReseau
  ask NoeudReseau [
         
         set [cibleReseau] of myself patch-here
        
        ]
  
  
  ;set lienReseau one-of arc-neighbors NoeudReseau; arcs with [end1 = NoeudReseau or end2 = NoeudReseau]
end

to initIndividualNoeudSuiveur

  set lienReseau 0
  ;;On a pas besoin ni de lienReseau, ni de NoeudReseau car on va sur un noeud qui initialisera cible
  set NoeudReseau 0
  
end

to CptIndividu_init 
   
    if hideIndividuals [set hidden? true]
    
    ;;Le fait de reprendre des individu existant necessite un nettoyage correct de toute les variables à zero, donc aucun résidu
    set noeudActuel 0
    set cible 0
    set has_muted false
    set estMort false
    set has_statebase 0
    set cpteurLastVisite 0
    set compteurTemps 0
    set compteurDistance 0
    set vitesse vitesseMax
    set actualLink nobody
    set vitesseLinkActuel 1
    let nb random-float 1
    set ticketService 0
    set waitingInWhatService 0
    set memEvtChemin []
    set icolor 0
    set chercheReseau? true
    
    set patchResidence one-of patches with [coeffPop2 >= nb and coeffPop1 <= nb] ; permet d'affecter la pop au pro rata ad hoc
    
    if patchResidence != nobody
    [
    
    setxy  [pxcor] of patchResidence [pycor] of patchResidence
    
    ;; On initialise l'identifiant du quartier pour l'individu
    set idQ [idQuartier] of patch-here
    ;ask quartiers with [id = [idQ] of myself] [set compteurDeparts compteurDeparts + 1]
    
    ;; On comptabilise la creation d'un nouvel individu dans le quartier
    ask quartiers with [id = [idQ] of myself] 
      [
        set nb_individu_tot nb_individu_tot + 1
        ;; On stocke le who du quartier d'origine pour plus de rapidite dans les futures interrogations
        set [idQuart] of myself [who] of self
      ]

    set cheminParcouru []
    set cheminFutur []
    let nb2 random-float 1

    let choixCible random-float 1
    let temp2 []
    
    set type_individual 0 ; l'individu n'a pas de type pour le moment, donc indiferencie
    set has_find_node false ; l'individu n'est pas sur un noeud par défaut
    set has_find_service false ; l'individu n'a pas trouvé de service par défaut
    
    ;Si le quartier ne compte pas de chemin, alors on a que des explorateurs ...
    ifelse (nb2 > fractionExplorateurs and cptQuartier_countChemin quartier idQuart != 0)
    [;; L'individu est un suiveur
      set icolor 50
      initIndividualSuiveur
      initIndividualNoeudSuiveur
    ]
    [;L'individu est un explorateur
      set icolor 10
      initIndividualExplorateur
      initIndividualNoeudExplorateur
    ]
    
    
    
    set besoin besoinInitial
    ;if labels? [set label floor besoin]
    
    set satisfaction 0

    file-open fichier_format
  while [not file-at-end?]
    ;; lecture des lignes du fichier et création du noeud
 [   let items read-from-string (word "[" file-read-line "]")
    set color item 6 items - icolor
    set shape item 7 items
    set size item 8 items / 1.2
 ]
file-close]

end


to CptIndividu_avancerReseau [direction]

  if (distance-nowrap direction != 0) 
  [
      set heading towards direction 
  ]
  

  fd 1
  
  ;;On comptabilise la vitesse
  set compteurDistance compteurDistance + 1 * vitesse
  set compteurPasTotal compteurPasTotal + vitesse

end

;- - - - - -

to CptIndividu_setOrientationReseauGoToNode [t_noeudReseau]

    set cible t_noeudReseau
    set vitesseLinkActuel 1
    
end

to CptIndividu_setOrientationReseauGoToPatch [idIndividual]

    set cible cibleReseau
    set vitesseLinkActuel [value] of (lienReseau)

end

to CptIndividu_setOrientationReseauAtNode [idIndividual]

    ;set cibleReseau (item 0 (cheminFutur))
    set cible (item 0 (cheminFutur))
    
    set cheminFutur (but-first (cheminFutur))
    set cheminParcouru fput cibleReseau cheminParcouru
    
    ;La vitesse pour rejoindre le noeud est fixé arbitrairement à 1
    set vitesseLinkActuel 1
    
end

to-report CptIndividu_WatchReseau [cibleRes]

  ifelse type_individual = 1
  [ ; on recherche un patch pour les explorateurs
    if (patch-here = cibleRes)
    [
      report false
    ]
  ][ ; on recherche un noeud pour les suiveurs
    if (one-of nodes-here = cibleRes)
    [
      report false
    ]
  ]
  
  report true

end

; gestion de l'orientation des individuals
; CAD definition d'une nouvelle cible de deplacement
;
to CptIndividu_setOrientation [idIndividual]

  ;; Si premier tour de l'individu alors on initialise les noeuds selon son type
 ;print "deplacement > cptpas" 
 ;show [compteurTemps] of idIndividual
  
  ;;La cible du tour précédent (qui existe forcement car has_terminate_true = true) devient le noeud actuel
  ; sauf dans le cas ou on se raccroche au reseau

    set Noeudactuel cible
  
  ; On reinitialise le marqueur dorientation a faux, jusqua la prochaine fois qu'on atteint le noeud tiré
  set [has_find_node] of idIndividual false
      
      ifelse ([type_individual] of idIndividual = 0) ;; indiferencie
      [
        ;print "> deplacement > probleme d'individu indifferencie"
      ]
      [
      
      ;; Si il y'a un chemin futur qui est egale a zero, on est un "pur" explorateur
      if ([type_individual] of idIndividual = 1 and length ([cheminFutur] of idIndividual) = 0) 
      [
        ;; Initialisation des noeuds sans avoir un chemin défini
        ;; On prend le noeud a distance minimum le premier coup
        
        ;; On ne suit plus un chemin particulier
        set idCheminEnCours 0
                       
        ;;print (word "tirage expl " [who] of idIndividual " > " cible) 
        tirage_direction_alea idIndividual

      ]
      
      if ([type_individual] of idIndividual = 2 ) ;; suiveur
      [
               ifelse (length [cheminFutur] of idIndividual > 0)[
                 
                 set [cible] of idIndividual item 0 [cheminFutur] of idIndividual 
                 ;show word "nouvelle cible chemin suiveur " [cible] of idIndividual
               ][
                 ;; Si il y'a un chemin futur qui est différent de zero, c'est que l'on est un ancien suiveur, 
                 ;; on continue a avancer selon le chemin prédéfini tant que length > 0
                 ;; FIXME : Ajouter un evt mutation ?
                 ;; On ne suit plus un chemin particulier
                 
                 ;show "Mutation suiveur > explorateur"
                CptIndividu_changeToExplorateur
                 
                 tirage_direction_alea idIndividual
               ]
       
      ];fin suiveur
      ]
    
    ;; On recupere la vitesse du link qui arrive,ca fait partie des comportements préalable à l'état avancer  
    ;; On aurait pu décider également de renseigner la vitesse quand has_node = true
    
      ;; on définit la vitesse sur le link a venir
      ifelse is-node? cible and is-arc? arc [who] of Noeudactuel [who] of cible[
        set vitesseLinkActuel [value] of (arc [who] of Noeudactuel [who] of cible)
      ][
        set vitesseLinkActuel 1
      ]
end

to CptIndividu_changeToExplorateur
 set has_muted true 
 set idCheminEnCours 0
 set type_individual 1
end

 to tirage_direction_alea [idIndividual]; nouvelle orientation aléatoire parmis les noeuds adjacents (liaison par un link)
 
   ask Noeudactuel
    [
      ;;On recupere les noeud alentours au noeudActuel
      let ListeNoeud link-neighbors
      let noeudPrec 0
      let cibleTemp 0
      let sortie false

      ifelse (length([cheminParcouru] of idIndividual) > 1) 
      [
        ;;  On retranche de 2, car on ne demarre l'algo que quand le chemin parcouru est >1
        set noeudPrec  item (length([cheminParcouru] of idIndividual) - 2) [cheminParcouru] of idIndividual
        ;show word "noeudPrec > " noeudPrec
      ][
        set noeudPrec self
      ]
      
       while [sortie = false]
       [
        set cibleTemp one-of ListeNoeud;
          if (cibleTemp != noeudPrec)
          [
            ;show word "cible choisi >" cibleTemp
            set sortie true
          ]
       ]
      
      if (sortie = true)
      [
        set [cible] of idIndividual cibleTemp
      ]
      
    ]  
 end
 
to CptIndividu_avancer [direction idIndividual]
; permet de faire avancer l'individual dans la direction de l'agent indiqué en paramétre
; et de tester la présence de services sur les actualLinks dont on est sur qu'ils en possèdent

    if (distance-nowrap direction != 0) 
    [
      set heading towards direction
    ]
    
    ifelse (distance-nowrap direction >= vitesse )
      [
      ;;FIXME : FAIRE UN TEST PLUS FIN : Soit je vois une personne et je teste le heading, soit je vois un service et je teste sans heading
      ifelse count individuals in-cone vision angleVision with [abs (heading - [heading] of myself) < toleranceAngle] > 1 [
      fd (vitesse / vitesseLinkActuel) / factCongestion
        set compteurDistance compteurDistance + (1 / vitesseLinkActuel) * vitesse / factCongestion
        set compteurPasTotal compteurPasTotal + vitesse
       ;ask patches in-cone 20 33 [set pcolor red]
       ;show word "avancer congestion = true > " ((vitesse / vitesseLinkActuel) / factCongestion)
       ][
        fd vitesse / vitesseLinkActuel
        set compteurDistance compteurDistance + (1 / vitesseLinkActuel) * vitesse
        set compteurPasTotal compteurPasTotal + vitesse
        ;show  word "avancer normal = true > " (vitesse / vitesseLinkActuel)
        ;show  word "vitesse > " vitesse
        ;show word "vitesse link actuel > " vitesseLinkActuel
        ]
      ]
      [
       ;; FIXME : A VERIFIER
       fd distance-nowrap direction  ;; si l'objectif est plus proche que le pas : on avance jusqu'à l'objectif
       set compteurPasTotal compteurPasTotal + distance direction
       set compteurDistance compteurDistance + (1 / vitesseLinkActuel) * vitesse
;       ifelse (is-patch? direction) [
;         setxy  [pxcor] of direction [pycor] of direction
;       ][
        setxy  [Xcor] of direction [Ycor] of direction
  ;     ]
      ;show  "avancer nowrap = true > " 
      ]
      

end

;; Consulter la liste des services mémorisé sur le chemin
;; FIXME : passer la mémoire en agent; on pourra meme memoriser les relations interIndividu ...
to-report CptIndividu_AskMemories [idIndividual typeEvt typeCmd]
  
  let t_memEvtChemin [memEvtChemin] of idIndividual
  let t_resultCmd []
  
  if length(t_memEvtChemin) > 0
  [
      foreach (t_memEvtChemin) 
      [
        let readTypeEvt  item 0 (?)
        
        ;;FIXME : utiliser des methode pour new Commande a l'initialisation de l'individu
        if readTypeEvt = typeEvt 
        [
          if substring typeCmd 0 5 = "List:"
          [
           let newCmd (remove "List:" typeCmd)
             if newCmd = "AllService"
             [
               let service item 6 (?)
               set t_resultCmd fput service t_resultCmd
             ]
          ]
          
          if substring typeCmd 0 4 = "get:"
          [

            ;;FIXME : Pourquoi ne pas imaginer un get:who:initTargetChemin > cela simplifierai les choses... mais pour cela il faut un parseur de ligne de cmd
            let newCmd (remove "get:" typeCmd)
            
            if newCmd = "WhoCheminInit" [

              let cmd item 1 (?)             
              let type_ind item 2 (?)
              let whoChemin item 3 (?)
              let chemin item 4 (?)
              let service item 5 (?)
            
              if cmd = "InitTargetChemin" 
              [
                set t_resultCmd fput whoChemin t_resultCmd
              ]
              
            ]
            
            if newCmd = "ServiceCheminInit" [
             
              let cmd item 1 (?)
              let type_ind item 2 (?)
              let whoChemin item 3 (?)
              let chemin item 4 (?)
              let service item 5 (?)
            
              if cmd = "InitTargetChemin" 
              [
                set t_resultCmd fput service t_resultCmd
              ]
              
            ]
            
            
          ]
         
      ] 
     ]

   ]
   
  report t_resultCmd
end

to-report CptIndividu_watchService [idIndividual]

  ;; On ne fait pas la difference entre un service sur un noeud et un service sur la route, 
  ;; les différencié complique les choses et n'apporte au final pas grand chose, si au pire on veut savoir si le service est sur un noeud, on testera avec node-here.
  ;; FIXME : nous ne somme plus censé revisiter un service dont on vient de sortir, donc watchService doit consulter la mémoire des services pour eviter de rerentrer dans le service.
   
   if (any? services in-radius vision)
   [
     let t_listeService 0
     let t_listeServiceRadius services in-radius vision
     ;;pour le moment on considère que le choix se fait de visu au hasard entre les services dans le champ de vision
     ;;FIXME : Amélioration pour prendre le plus proche !!
     let oneOfServiceInRadius one-of t_listeServiceRadius
     
     ;;Est ce que le service que l'on voit est fermé?
     ifelse [has_close] of oneOfServiceInRadius = false
     [

       set waitingInWhatService [who] of oneOfServiceInRadius
     
       ;show word "oneOfServiceInRadius > " oneOfServiceInRadius
     
       ;On recupere la liste de service en mémoire 
       set t_listeService (CptIndividu_AskMemories idIndividual "evtShow" "List:AllService")
       ;show word "t_listeService > " t_listeService
     
       foreach (t_listeService)
       [
         ;Si on trouve un service qui correspond a la liste mémoire ...
         if (? = [who] of oneOfServiceInRadius)
         [
           ;show word "Le service est en mémoire ... " ?
           report false
         ]
       ]
        ;show  "Le service n'est pas en mémoire, on va le stocker ... " 
        report true 
     ][
       report false
      ]
  ]
  
  report false

end

to-report CptIndividu_watchPath [idIndividual noeudCible]

    let t_nodeHere false
    
    if any? nodes-here [
      ask nodes-here
      [
        if node-id = [node-id] of noeudCible
        [
          set t_nodeHere true
          ;set [Noeudactuel] of idIndividual [cible] of idIndividual ;? necessaire ici ?
        ]
      ]
    ]
    
    report t_nodeHere
    
    ;;FIXME : est on bien sur le service cherche au depart, ou est ce un nouveau ? > voir ci dessus
;    if any? services-here
;    [
;       ;set [Noeudactuel] of idIndividual [cible] of idIndividual ;? necessaire ici ?
;       set [has_find_node] of idIndividual true
;       set [has_find_service] of idIndividual 1
;    ]

end


;;FIXME : PAS ASSEZ GENERIQUE
;;COMPORTEMENT INDIVIDU PAR INDIVIDU ... PAS BON
to CptIndividu_savePathMemories [idIndividual noeudToSave nodeExist ServiceExist]

  if nodeExist = true and ServiceExist = false
  [
    ;show "_____________________________________"
    ;show "Node exist true, service exist false"
    ifelse length ([cheminParcouru] of idIndividual) != 0 
    [
      ; On doit tester ici que le noeud n'existe pas deja a n -1, effectivement, quand on sort d'un service, on repasse sur le noeud enregistré à l'entrée du service ...
      ifelse not (last [cheminParcouru] of IdIndividual = noeudToSave) [
        ;show "Noeud n'existe pas dans chemin parcouru"
        set [cheminParcouru] of idIndividual (lput noeudToSave [cheminParcouru] of idIndividual)
      ][
        ;show "Noeud existe deja dans chemin parcouru"
      ]
      
    ][
       set [cheminParcouru] of idIndividual (lput noeudToSave [cheminParcouru] of idIndividual)
    ]
  ]
  
  ; Si le noeud existe, que le service aussi, mais que le service n'est pas sur le noeud en réalité
  ; Si le noeud existe , on présupose que l'on est sur le noeud pour faire le test ...
  ;---------------------------------------------------------------------------------------------------
  ;---------------------------------------------------------------------------------------------------
  ;---------------------------------------------------------------------------------------------------
  ;---------------------------------------------------------------------------------------------------
  ;---------------------------------------------------------------------------------------------------
  ;; MODIFICATIONS POUR TENIR COMPTE DE LA DISPARITION DE LINKPROJ !!!
  ; ancien code:
  
;  if nodeExist = true and ServiceExist = true and any? services-here with [[who] of self = [waitingInWhatService] of idIndividual] = false
;  [
;    ;show "Node exist true, service exist true, service here false"
;    let t_linkProj 0
;    let t_noeudToSave 0
;    
;    ask service waitingInWhatService [ set t_linkProj linkProj ]
;    
;    if [end1] of linkProj = noeudToSave [ set t_noeudToSave [end2] of linkProj ]
;    if [end2] of linkProj = noeudToSave [ set t_noeudToSave [end1] of linkProj ]
;
;    set [cheminParcouru] of idIndividual (lput noeudToSave [cheminParcouru] of idIndividual)
;    set [cheminParcouru] of idIndividual (lput t_noeudToSave [cheminParcouru] of idIndividual)
;  ]
;; NOUVEAU CODE:
    if nodeExist = true and ServiceExist = true and any? services-here with [[who] of self = [waitingInWhatService] of idIndividual] = false
  [
    ;show "Node exist true, service exist true, service here false"
    let t_linkProj 0
    let t_noeudToSave 0
    
    ask service waitingInWhatService [ set t_linkProj linkservice ]
    
    if [end1] of t_linkProj = noeudToSave [ set t_noeudToSave [end2] of t_linkProj ]
    if [end2] of t_linkProj = noeudToSave [ set t_noeudToSave [end1] of t_linkProj ]

    set [cheminParcouru] of idIndividual (lput noeudToSave [cheminParcouru] of idIndividual)
    set [cheminParcouru] of idIndividual (lput t_noeudToSave [cheminParcouru] of idIndividual)
  ]
    ;---------------------------------------------------------------------------------------------------
  ;---------------------------------------------------------------------------------------------------
 
  
  ;FIXME : ne devrai arriver que si on desactive le radius
  ;;Idem qu'au dessus, mais le service est  bien sur le noeud
  if nodeExist = true and ServiceExist = true and any? services-here with [[who] of self = [waitingInWhatService] of idIndividual] = true
  [
    ;show "Node exist true, service exist true, service here true"
    set [cheminParcouru] of idIndividual (lput noeudToSave [cheminParcouru] of idIndividual)
  ]
  
  if nodeExist = false and ServiceExist = true
  [
    ;show "Node exist false, service exist true"
    let t_linkProj 0
    let t_noeudToSave 0
    
    ask service waitingInWhatService [ set t_linkProj linkProj ]
    ;show word "length chemin parcouru " length( [cheminParcouru] of IdIndividual )
    
    ifelse length( [cheminParcouru] of IdIndividual ) = 0 
    
    [;;Cas ou on tombe directement sur un service apres apparition > On stocke les deux noeud fct de la distance
     ;; Realiser le test de distance pour se raccrocher au plus pres  
;     set [cheminParcouru] of idIndividual (lput [end1] of linkProj [cheminParcouru] of idIndividual)
;     set [cheminParcouru] of idIndividual (lput [end2] of linkProj [cheminParcouru] of idIndividual)
;     
;     
; dans ce cas on stocke le premier lien prévu, même si celui ci n'est pas effectué
     set [cheminParcouru] of idIndividual (lput [end1] of [lienReseau] of idIndividual [cheminParcouru] of idIndividual)
     set [cheminParcouru] of idIndividual (lput [end2] of [lienReseau] of idIndividual  [cheminParcouru] of idIndividual)
    ]
    [
      ; si on tombe sur un service alors qu'un noeud a deja été parcouru et que l'on est pas sur un noeud, et que ce noeud n'existe pas deja dans la liste cheminParcouru
     if not (last [cheminParcouru] of IdIndividual = noeudToSave) [
        set [cheminParcouru] of idIndividual (lput noeudToSave [cheminParcouru] of idIndividual)
     ]
    ]
  ]
  
  ;show word "chemin parcouru" [cheminParcouru] of idIndividual
  
end
 
to CptIndividu_loadPathMemories [nodeExist]

  ;;Si on est un suiveur, on réduit la mémoire du cheminFutur de 1 a chaque fois que l'on passe un nouveau noeud
  ;;FIXME : on refait un test pour verifier que le noeud est bien le bon ? cheminParcouru = CheminFutur item 0
  
  ;;Si il y'a un chemin futur ;FIXME : test utile ? 
  if (CheminFutur != 0 and nodeExist = true)
  [
    ;;C'est un tableau différent de zero
    if (length CheminFutur > 0)
    [
      ;; On réduit le tableau de 1
      
      set CheminFutur but-first CheminFutur
      
    ]
  ]

end
     
;; ------------------------
;; Procedures services
;; ------------------------
to-report CptService_askTraitementFileAttente [numeroAttente]

  ;La file d'attente répond au demande tout les n ticks, chose que l'on peut régler via un curseur
  if (ticks mod PeriodeTraitementFileAttente = 0)
  [
    if (numeroAttente = numeroFileIndividuSuivant)
    [
        report true
    ]
  ]
  report false  
  
end

to-report CptService_repondBesoin [ individu ]
  
  let t_satisfaction 0
  let t_nombreUnite MaxUnitStockDelivreParDemande
  let t_min stock
  if stock > t_nombreUnite [set t_min t_nombreUnite]
  ;;--------------------- < ???
  ifelse (stock > 0)
  [    
       ;;FIXME : Rajouter la gestion des paquet délivré par le service
       if (t_min >= [besoin] of individu)
       [
          ;show "je pioche dans le stock"
          set stock (stock - floor [besoin] of individu)
          set soldUnits (soldUnits + floor ([besoin] of individu))
          set [besoin] of individu 0
          set t_satisfaction 1
       ]
       
       if (t_min < [besoin] of individu)
       [
         set stock (stock - t_min)
         set soldUnits (soldUnits + t_min)
         ;show "le stock ne me suffit pas"
         set t_satisfaction ([besoin] of individu / t_min)
         set [besoin] of individu ([besoin] of individu - t_min)
         set besoin_insatisfait besoin_insatisfait + ([besoin] of individu - t_min)
       ]
  ][

     set t_satisfaction 0
     set besoin_insatisfait besoin_insatisfait + ([besoin] of individu)
  ]
  
  ;On renvoie le pourcentage du besoin
  ;show word "satisfaction = " t_satisfaction
  report t_satisfaction
  
  
end 

to CptService_closeService
    
    without-interruption
    [
      ;;Permet de verifier que l'on est bien dans un état fermé ...       
      ifelse (tailleFileAttente != 0 )
      [
        ;; On demande aux gens de sortir de l'etat d'attente...
        ;; FIXME : Faire un comportement LeaveFileAttente dans la prochaine version... 
         ask individuals with [waitingInWhatService = [who] of myself]
         [
              set has_statebase  0
              set has_find_service false
              set waitingInWhatService 0
         ]
         
         die
        
      ][
        die
      ]
      
      
    ]
end

to gestionService  ;; gestion des stocks des services et de l'apetit des individuals.

  ; set cible noeudSuivant
  ;let idIndividual 0
  ;show "gestion service : "
  let min_ticket 0
  let min_who 0
   
  ;On tire le prochain individu a etre appelé, seulement dans la file d'attente
   ask individuals with [waitingInWhatService = [who] of myself and has_statebase = 2] 
   [
     if (([ticketService] of individual [who] of self < min_ticket) Or min_ticket = 0)
     [
       set min_ticket [ticketService] of individual [who] of self
       set min_who [who] of self
     ]
   ]
   
   if min_who != 0
   [
      set numeroFileIndividuSuivant min_ticket
   ]

end

;; ---------------------
;; Procedures gestion graphique
;; ---------------------

;; Formatage visuel du graphe.

to formatage_graphe
  ;; Ouverture du fichier
  file-open fichier_format
  while [not file-at-end?]
    ;; lecture des lignes du fichier et création du noeud
 [   let items read-from-string (word "[" file-read-line "]")

  ask nodes
  [
    set color item 0 items
    set shape item 1 items
    set size item 2 items
  ]
  

  ask services 
  [
    set color item 3 items
    set shape item 4 items
    set sizeInit item 5 items    
    ;set size sizeInit
   ;set size 3 * sizeInit * (1 - exp (- stock / sizeInit)) / 2 + sizeInit / 2
   ; set size stock
   updateSizeServices
  ]

;  ask individuals 
;  [
;    set color item 6 items
;    set shape item 7 items
;    set size item 8 items
;  ]
  
  ask links
  [
    set color item 9 items
    set thickness item 10 items
  ]
 ]
file-close
end

to updateSizeServices
  let lambda 1 / 2
  let mu 1 / (3 * sizeInit)
  set size 1.3 * ((2 - lambda) * sizeInit * (1 - exp (- mu * stock)) + sizeInit * lambda)
end

to cptService_recalculProd
;------------------------------- FONCTION A CHANGER
  let temp_C soldUnits + besoin_insatisfait
  let temp_F 0
  let temp_N 0
  if (prix_vente + perte_insatisfait + perte_invendu) != 0 [
    set temp_F (prix_vente + perte_insatisfait) / (prix_vente + perte_insatisfait + perte_invendu)
  ]
  set argent argent + soldUnits * prix_vente; - perte_insatisfait * besoin_insatisfait - perte_invendu *  stock
  ;Calcul fonction du nombre de satisfait, des insatisfait, du stock invendu, 
  set achalandage (lput temp_C achalandage)

  if tempsDepuisOuverture / periodeRecalculProd > nIterMemorized [set achalandage (sublist achalandage 1 ((length achalandage)))]

  if temp_F != 0 [
    ifelse round (temp_F * length achalandage) < length achalandage [
      set temp_F round (temp_F * length achalandage)
    ][
      set temp_F length achalandage - 1
    ]
    set temp_N item temp_F sort achalandage
;    ifelse stock >= temp_N [
;      set temp_N 0
;    ][
;      set temp_N temp_N - stock
;    ]
  ]
    ifelse argent >   prix_unite * (temp_N) [
      set stock temp_N
      set argent argent - prix_unite * (temp_N)
    ][
      ifelse argent >= 0 [
        set stock floor (argent / prix_unite)
        set argent argent - prix_unite * stock
      ][
        set stock 0
        set argent 0
      ]
    ]
    
  
    ;set argent argent - prix_unite * (temp_N)
    set argent argent - loyer
    
   ; paiement du loyer; si impossible, fermeture
    if (argent < 0 and length achalandage = nIterMemorized)
    [
     set has_close true 
    ]

   
  ;set stock stock + temp_N
  if labels? [set label floor argent]
  
;  CptService_updateStockEfficace (stock + 1)
  set besoin_insatisfait 0
  set soldUnits 0
  updateSizeServices
end


;;;; ----------------------------------------------------------------------------
;;;; Met ? jour les attributs du service
;;;; ----------------------------------------------------------------------------
;to updateStock
;;  ifelse simulTime / serviceTime > nIterMemorized
;;  [
;    ;update liste et on calcule la moyenne et on update le stock
;    ;update liste
;    set lastSales (lput soldUnits lastSales)
;    if tempsDepuisOuverture / serviceTime > nIterMemorized [set lastSales (sublist lastSales 1 ((length lastSales)))]
;    ;on update le stock si on a vendu assez
;    ifelse (soldUnits > (parRenewalDecision * stock)) 
;    [
;      ;set stock floor (parRenewal * (stock + (mean lastSales)))
;      if lastSales != []
;      [
;      ;set stock floor (stock + parRenewal * (mean lastSales))] ;formule gérant le fonctionnement des boulangeries
;      set stock stock + floor (parRenewal * (mean lastSales))] ;formule gérant le fonctionnement des boulangeries
;      ;choix à faire sur la formuler
;      set nIterWithoutRenewals 0
;    ]
;    [
;      set nIterWithoutRenewals (nIterWithoutRenewals + 1)
;    ]
;    updateSizeServices
;    ;set size sizeInit * (stock / max [stock] of services)
;    ;set size stock
;    ;set size 3 * sizeInit * (1 - exp (- stock / sizeInit)) / 2 + sizeInit / 2
;  set soldUnits 0
;  ;  evalIfHaveToClose ;; Le service commence par d?terminer si il doit fermer ou non
;    ;if (nIterWithoutRenewals >= parIterWithoutRenewals * size)
;    ;if (length lastSales = nIterMemorized and sum lastSales < Seuil_Fermeture)
;    if (sum lastSales < Seuil_Fermeture)
;    [
;      die
;    ]
;end

;to mettre-a-jour-graphiques
;
;set-current-plot "Ventes Totales"
;  set-plot-x-range 0 1
;  set-plot-y-range 0 1
;    plotxy (simulTime / 600) sum [sum lastSales] of services
;if sum [compteurDeparts] of quartiers != 0 and sum [txpop] of quartiers with [compteurDeparts > 0] != 0 and sum [txpop * bonheur] of quartiers with [compteurDeparts > 0] != 0[
;set-current-plot "Taux de Satisfaction (%)"
;  set-plot-x-range 0 1
;  set-plot-y-range 0 1
;    plotxy (simulTime / 600) 100 * (sum [txpop * txSatisfaction] of quartiers with [compteurDeparts > 0] / sum [txpop] of quartiers with [compteurDeparts > 0])
;
;set-current-plot "Distance (mètres)"
;  set-plot-x-range 0 1
;  set-plot-y-range 0 1
;    plotxy (simulTime / 600) 10 / (sum [txpop * bonheur] of quartiers with [compteurDeparts > 0] / sum [txpop] of quartiers with [compteurDeparts > 0])
;]
;end

to mettre-a-jour-graphiques

if sum [argent] of services > max_graphe_richesse [set max_graphe_richesse round sum [argent] of services + 1]
set-current-plot "Richesse Totale"
  set-plot-x-range 0 ticks + 1
  set-plot-y-range 0 max_graphe_richesse + 1
    plotxy ticks sum [argent] of services
if sum [txpop] of quartiers with [nb_individu_tot > 0] != 0 [


    
 set graphe_temps (sum [txpop * best_temps] of quartiers with [nb_individu_tot > 0] / sum [txpop] of quartiers with [nb_individu_tot > 0])
 if graphe_temps > max_graphe_temps [set max_graphe_temps round (graphe_temps) + 1]
 
set-current-plot "Temps Moyen"
  set-plot-x-range 0 ticks + 1
  set-plot-y-range 0 max_graphe_temps + 1
    plotxy ticks graphe_temps
    
;set-current-plot "Histogramme Temps"

set graphe_temps2 (max [best_temps] of quartiers with [nb_individu_tot > 0] - min [best_temps] of quartiers with [nb_individu_tot > 0])
 if graphe_temps2 > max_graphe_temps2 [set max_graphe_temps2 round (graphe_temps2) + 1]
set-current-plot "Inégalité Temps"
;  ;set-plot-x-range 0 1
;  ;set-plot-y-range 0 1
;    histogram  [best_temps] of quartiers with [nb_individu_tot > 0]    
  set-plot-x-range 0 ticks + 1
  set-plot-y-range 0 max_graphe_temps2 + 1
    plotxy ticks graphe_temps2
]
end


;; IMPORT ZONES :Lecture dans le fichier texte des valeurs à 
;; implémenter pour l'attribut "idQuartier" de chaque patches
to import-iris
  file-open fichier_grille
  let patch-idQuartier []
  if not file-at-end? 
  [
    set patch-idQuartier file-read
    file-close
    (foreach sort patches patch-idQuartier
     [
       ask ?1 
       [ 
         set idQuartier ?2
       ]
     ]
    )
    setupTemp
  ]
end

;; Nettoyage de la fenêtre
;; Attribution d'une couleur aux patches ayant une valeur différente de -9999
to setupTemp
  cd
  ct

  ask patches
  [
    ifelse idQuartier = -9999 
    [
      ;set pcolor white
      set pcolor backgroundColor
    ]
    [
      set pcolor backgroundColor
    ]
  ]
end


;---------------------------
; Importation des services
;---------------------------

to import-service
  ;set-default-shape services "house"
  ;; Ouverture du fichier
  file-open fichier_service

  while [not file-at-end?]
  [
    ;; lecture des lignes du fichier
    let items read-from-string (word "[" file-read-line "]")
    create-services 1 
    [
      ;; pour chaque ligne, on extrait 4 informations : identifiant du service, coord en X et coord en Y
      set id item 0 items
      set xcor item 1 items
      set ycor item 2 items
      ;set color red
      ;set size 10
      
    ]
  ]
  file-close
end

;--------------------
;Exécution de l'importation du RESEAU
; 1°) import-noeud
; 2°) import-lien
;--------------------

to import-reseau
  set-default-shape nodes "circle"
  ; ca
  import-noeud
  import-lien
end

;; Cette procédure lit dans le fichier texte les NOEUDS du graphe
to import-noeud
  ;; Ouverture du fichier
  file-open fichier_noeud
  while [not file-at-end?]
  [
    ;; lecture des lignes du fichier et création du noeud
    let items read-from-string (word "[" file-read-line "]")
    create-nodes 1 [
    ;; pour chaque ligne, on extrait 3 informations : identifiant du noeud, coordonnée x et coordonnée y
      set node-id item 0 items
      set xcor    item 1 items
      set ycor    item 2 items
      ;; Optionnel => Mettre en commentaire la ligne suivante si pas d'attribut dans le fichier texte
      set node_attribut item 3 items
      ;set color green
      ;; si l'importation concerne des polygones, on ne fait pas apparaitre les noeuds
      ;set size 3 
    ]
  ]
  file-close
end


;; Cette procédure lit dans le fichier texte les LIENS du graphe
to import-lien
  ;; Ouverture du fichier
  file-open fichier_lien

  while [not file-at-end?]
  [
    ;; lecture du service de départ et du service d'arrivée dans le fichier texte
    ;; Création du lien entre les services
    let items read-from-string (word "[" file-read-line "]")
    let nodezero get-node (item 0 items)
    let nodeun get-node (item 1 items)
    let valuearc (item 2 items)
    
    ask nodezero
    [
      create-arc-with nodeun
    ]
    
    ;;On initialise la valeur de l'arc
    ask arc [who] of nodezero [who] of nodeun
    [ 
      set value valuearc
    ]
  ]
  file-close
end

;; Renvoie l'identifiant du noeud recherché
to-report get-node [_idNode]
  report one-of nodes with [node-id = _idNode]
end

;; Lit le coeff associ? 

to import-forme 

if fichier_forme = "paris12"
  [
      set fichier_grille "paris12_quartiers.txt"
      set fichier_service "paris12_services_P.txt"
      set fichier_noeud "paris12_rues_P.txt"
      set fichier_lien "paris12_rues_L.txt"
      set fichierPop "paris12_quartier_pop.txt"
  ]
if  fichier_forme = "manhattan"
  [
      set fichier_grille "manhattan_quartiers.txt"
      set fichier_service "manhattan_services_P.txt"
      set fichier_noeud "manhattan_rues_P.txt"
      set fichier_lien "manhattan_rues_L.txt"
      set fichierPop "manhattan_quartiers_data.txt"
  ]
  if  fichier_forme = "bastide"
  [
      set fichier_grille "bastide_quartiers.txt"
      set fichier_service "bastide_services_P.txt"
      set fichier_noeud "bastide_rues_P.txt"
      set fichier_lien "bastide_rues_L.txt"
      set fichierPop "bastide_quartiers_data_unif.txt"
  ]
  if  fichier_forme = "bastide_unif_vide"
  [
      set fichier_grille "bastide_quartiers.txt"
      set fichier_service "all_services_empty.txt"
      set fichier_noeud "bastide_rues_P.txt"
      set fichier_lien "bastide_rues_L.txt"
      set fichierPop "bastide_quartiers_data_unif.txt"
  ]
  if  fichier_forme = "poly_mono"
  [
      set fichier_grille "poly_quartiers.txt"
      set fichier_service "all_services_empty.txt"
      set fichier_noeud "poly_rues_P_poly.txt"
      set fichier_lien "poly_rues_L_poly.txt"
      set fichierPop "poly_quartier_pop_cercles_mono.txt"
  ]
  if  fichier_forme = "poly_poly"
  [
      set fichier_grille "poly_quartiers.txt"
      set fichier_service "all_services_empty.txt"
      set fichier_noeud "poly_rues_P_poly.txt"
      set fichier_lien "poly_rues_L_poly.txt"
      set fichierPop "poly_quartier_pop_cercles_poly.txt"
  ]
end



to createQuartiers

  let quartiersId []
  let coeffQuartiers []
  let numPatchesByQuartier 0
  let coeff 0
  let blob 0
  
  ; lire le fichier et remplir les listes quartiersId et coeffQuartier
  file-open fichierPop
  while [not file-at-end?]
  [
    let items read-from-string (word "[" file-read-line "]")
    set blob item 0 items
    set coeff item 1 items
    set quartiersId lput blob quartiersId
    set coeffQuartiers lput coeff coeffQuartiers
  ]
  file-close
  
  ;; On attribue ? chaque patch un coeff de population 
  
  (foreach quartiersId coeffQuartiers
  [
    create-quartiers 1 [
      set id ?1
      set txPop ?2
      ; lignes à utiliser pour "matérialiser" les quartiers (utile pour le debugging)
      let temp one-of patches with [idQuartier = ?1]
      set hidden? true
      setxy [pxcor] of temp [pycor] of temp
      set compteurDeparts 0
      set nb_individu_tot 0
      set color yellow
      set size 6
      set shape "circle"
    ]
    ;set loading loading + 1
  ]
  )
  
end

to setPopToPatches

  let quartiersId []
  let coeffQuartiers []
  let numPatchesByQuartier 0
  let coeff 0
  let blob 0
  
  ; lire le fichier et remplir les listes quartiersId et coeffQuartier
  file-open fichierPop
  while [not file-at-end?]
  [
    let items read-from-string (word "[" file-read-line "]")
    set blob item 0 items
    set coeff item 1 items
    set quartiersId lput blob quartiersId
    set coeffQuartiers lput coeff coeffQuartiers
  ]
  file-close
  
  ;; On attribue ? chaque patch un coeff de population 
  let temp 0
  (foreach quartiersId coeffQuartiers
  [
    set numPatchesByQuartier (count patches with [idQuartier = ?1]) 
    ask patches with [idQuartier = ?1]
    [
      set coeffPop1 temp
      set coeffPop2 temp + (?2 / numPatchesByQuartier)
      set temp coeffPop2
      
    ]
  ]
  )
  
end

to affichage
obs_showFrequenceChemin
cptQuartiers_update_bestchemin
mettre-a-jour-graphiques
ifelse temps-on-density-off [
showTemps
][
showDensity
]
end

to showDensity

ask quartiers [
   let temp self
   let t_patch one-of patches with [idQuartier = [id] of temp]
   let temp_p backgroundColor + 10 - 7 * [txpop] of temp / max [txpop] of quartiers
   if (abs([pcolor] of t_patch - temp_p) > seuil_affichage) [
    ask patches with [idQuartier = [id] of temp] [
      set pcolor temp_p
      ;set loading loading + 1
    ]
  ]
]

end

to showTemps
if any? quartiers with [best_temps > 0] [
  ask quartiers [
    let temp self
     let t_patch one-of patches with [idQuartier = [id] of temp]
    let temp_p backgroundColor + 13 + 7 * [best_temps] of temp / max [best_temps] of quartiers
    if (abs([pcolor] of t_patch - temp_p) > seuil_affichage) [
       ask patches with [idQuartier = [id] of temp] [
        set pcolor temp_p
      ]
    ]
  ]
]
end

to-report projLink [tortue lien etat]

let x0 [pxcor] of tortue
let y0 [pycor] of tortue
let pointA [end1] of lien
let pointB [end2] of lien
let xA [xcor] of pointA
let yA [ycor] of pointA
let xB [xcor] of pointB
let yB [ycor] of pointB
;; Calcul des coordonn?es de la projection
let xH ((x0 - xA)*(xB - xA)+(y0 - yA)*(yB - yA))*(xB - xA)/((xB - xA)*(xB - xA)+(yB - yA)*(yB - yA)) + xA
let yH ((x0 - xA)*(xB - xA)+(y0 - yA)*(yB - yA))*(yB - yA)/((xB - xA)*(xB - xA)+(yB - yA)*(yB - yA)) + yA
let prodscal (xH - xA) * (xH - xB) + (yH - yA) * (yH - yB)
if prodscal > 0 [
  ifelse (((xA - xH) * (xA - xH) + (yA - yH) * (yA - yH)) < ((xB - xH) * (xB - xH) + (yB - yH) * (yB - yH))) [
    set xH xA
    set yH yA
    if etat = 2 [
      report pointA
    ]
  ]
  [
    set xH xB
    set yH yB
    if etat = 2 [
      report pointB
    ]
  ]
]

;; Projection
if (etat = 1) [
set [xcor] of tortue xH
set [ycor] of tortue yH
]
ifelse (etat = 2) [
  report patch xH yH
][
  report (x0 - xH)*(x0 - xH)+(y0 - yH)*(y0 - yH)
]
end

to-report projection [tortue etat]

let temp 0
let best 4 * max-pxcor * max-pycor
let lienne 0
ask links [
  set temp projLink tortue self 0
  if (temp < best) [
    set best temp
    set lienne self
  ]
]
;show word "la tortue" tortue
;show word "est projetee sur" lienne
;set best projLink tortue lienne etat
ifelse etat = 2 [
  report projLink tortue lienne 2
 ][
  report lienne
 ]
end

to projPatches
  set coordProj (projection self 2)
  if  is-node? coordProj 
  [
    ;show coordProj
  ]
  set linkProj (projection self 3)
end

to loadProjPatches
ask patches [
  projPatches
  ;set loading loading + 1
  ;set pcolor red
]

end


@#$#@#$#@
GRAPHICS-WINDOW
227
10
819
572
-1
-1
1.43
1
10
1
1
1
0
0
0
1
0
406
0
371
0
0
1
ticks

CC-WINDOW
5
1765
1148
1860
Command Center
0

BUTTON
847
70
1084
207
Start
if not pause? [go]\nplacerBoulangerie
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
285
1602
459
1635
stopTime
stopTime
5
50
18
1
1
NIL
HORIZONTAL

SLIDER
407
1238
580
1271
stockInitial
stockInitial
1
100
10
1
1
NIL
HORIZONTAL

SLIDER
394
1199
567
1232
nIterMemorized
nIterMemorized
1
100
12
1
1
NIL
HORIZONTAL

SLIDER
32
1361
206
1394
serviceTime
serviceTime
1
1000
51
1
1
NIL
HORIZONTAL

PLOT
9
276
216
411
Richesse Totale
ticks
richesse
0.0
10.0
0.0
100.0
true
false
PENS
"default" 1.0 0 -16777216 true

SLIDER
288
1366
462
1399
besoinInitial
besoinInitial
0
10
1
1
1
NIL
HORIZONTAL

SLIDER
287
1567
461
1600
parFatigue
parFatigue
0
1
0
0.01
1
NIL
HORIZONTAL

PLOT
9
152
217
275
Inégalité Temps
ticks
différence temps
0.0
50.0
0.0
10.0
true
false
PENS
"default" 1.0 0 -16777216 true
"21" 1.0 0 -2674135 true
"22" 1.0 0 -13345367 true
"23" 1.0 0 -10899396 true
"24" 1.0 0 -8630108 true
"25" 1.0 0 -5825686 true
"26" 1.0 0 -2064490 true
"27" 1.0 0 -1184463 true
"28" 1.0 0 -11221820 true

CHOOSER
891
1492
1072
1537
fichier_grille
fichier_grille
"paris12_quartiers.txt" "manhattan_quartiers.txt" "bastide_quartiers.txt" "poly_quartiers.txt" "carres25.txt"
0

BUTTON
11
537
88
570
reset
ca\nsetup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

INPUTBOX
31
1444
211
1504
backgroundColor
98
1
0
Color

CHOOSER
891
1538
1071
1583
fichier_service
fichier_service
"paris12_services_P.txt" "all_services_empty.txt" "manhattan_services_P.txt" "bastide_services_P.txt" "bastide_service_centre_P.txt" "bastide_service_CP_P.txt" "bastide_service_2ec_P.txt"
1

CHOOSER
890
1584
1071
1629
fichier_noeud
fichier_noeud
"paris12_rues_P.txt" "poly_rues_P_mono.txt" "poly_rues_P_poly.txt" "manhattan_rues_P.txt" "bastide_rues_P.txt" "carre_rues_P.txt"
0

CHOOSER
891
1630
1071
1675
fichier_lien
fichier_lien
"paris12_rues_L.txt" "paris12_rues_L_u.txt" "poly_rues_L_mono.txt" "poly_rues_L_poly.txt" "manhattan_rues_L.txt" "bastide_rues_L.txt" "carre_rues_L.txt"
1

CHOOSER
891
1676
1071
1721
fichierPop
fichierPop
"paris12_quartier_pop.txt" "manhattan_quartiers_data.txt" "bastide_quartiers_data_unif.txt" "poly_quartier_pop_cercles_unif.txt" "poly_quartier_pop_cercles_mono.txt" "poly_quartier_pop_cercles_poly.txt" "bastide_quartiers_data_CP.txt" "bastide_quartiers_data_estwest.txt" "bastide_quartiers_data_CPinv.txt" "carres25_pop_test.txt" "carre25_quartiers_data_unif.txt" "carre25_quartiers_data_estwest.txt" "carre25_quartiers_data_CP.txt" "carre25_quartiers_data_CPinv.txt"
0

TEXTBOX
37
1345
187
1363
SERVICES
11
0.0
1

TEXTBOX
59
1582
209
1600
INDIVIDUS
11
0.0
1

CHOOSER
30
1397
211
1442
fichier_format
fichier_format
"config_plant_and_rabbit.txt" "config_car_and_factory.txt" "config_arrows_and_stars.txt" "config_person_and_house.txt"
3

BUTTON
243
1641
373
1674
Creer des services
ifelse count services + 20 <= Nombre_Services_Max [createServices 20 0 false]\n[createServices Nombre_Services_Max - count services 0 false]\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
289
1405
461
1438
fractionExplorateurs
fractionExplorateurs
0
1
0.1
0.01
1
NIL
HORIZONTAL

BUTTON
258
1677
476
1710
Fermer un service
if any? services [ask min-one-of services [size] [die]]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

PLOT
9
10
220
151
Temps Moyen
ticks
temps d'accès
0.0
10.0
0.0
10.0
true
false
PENS
"default" 1.0 0 -16777216 true

MONITOR
11
481
212
526
Nombre de Boulangeries
count services
17
1
11

SWITCH
669
1123
813
1156
creer-on-fermer-off
creer-on-fermer-off
0
1
-1000

TEXTBOX
575
1618
696
1744
Manhattan & Bastide :\nmax-pxcor = 249\nmax-pycor = 249\nParis 12 : \nmax-pxcor =406\nmax-pycor = 371\nPolycentrique\nmax-pxcor = 399\nmax-pycor = 399
11
0.0
1

SWITCH
46
1543
149
1576
Labels?
Labels?
1
1
-1000

CHOOSER
26
1299
208
1344
fichier_forme
fichier_forme
"paris12" "manhattan" "bastide" "bastide_unif_vide" "poly_mono" "poly_poly"
0

BUTTON
381
1642
511
1675
Fermer tous les services
ask services [die]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SWITCH
54
1253
157
1286
config
config
1
1
-1000

BUTTON
17
1639
112
1672
startMovie
movie-start \"test6.mov\"
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
113
1639
207
1672
stopMovie
movie-close\nset isMakingMovie? false
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SWITCH
17
1673
207
1706
isMakingMovie?
isMakingMovie?
1
1
-1000

SWITCH
32
1601
174
1634
individusVisible?
individusVisible?
0
1
-1000

INPUTBOX
513
1444
645
1504
nbIndividuals
200
1
0
Number

MONITOR
940
10
990
55
Jour
1 + round(simulTime / 1200)\n\n
0
1
11

TEXTBOX
861
31
938
54
\\___/ = 100 m
11
0.0
1

INPUTBOX
512
1378
643
1438
Seuil_Fermeture
0
1
0
Number

INPUTBOX
716
1565
871
1625
Seuil_Reelu
100
1
0
Number

INPUTBOX
717
1500
872
1560
Seuil_Satisfaits
70
1
0
Number

INPUTBOX
716
1627
871
1687
Somme_par_Habitant
60
1
0
Number

INPUTBOX
716
1691
871
1751
Seuil_Entrepreneur
120
1
0
Number

INPUTBOX
519
1511
649
1571
Temps_Parents
50
1
0
Number

INPUTBOX
716
1436
871
1496
Nombre_Services_Max
10
1
0
Number

SWITCH
474
1580
683
1613
satisfaction-on-distance-off
satisfaction-on-distance-off
0
1
-1000

MONITOR
11
425
212
470
Nombre Maximum de Boulangeries
Nombre_Services_Max
17
1
11

SLIDER
403
1107
557
1140
gl_proportion_freq_dist
gl_proportion_freq_dist
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
270
1115
388
1148
factCongestion
factCongestion
1
100
4
1
1
NIL
HORIZONTAL

SLIDER
268
1238
362
1271
VitesseMax
VitesseMax
0
100
1
1
1
NIL
HORIZONTAL

SLIDER
270
1154
367
1187
angleVision
angleVision
0
50
7
1
1
NIL
HORIZONTAL

SLIDER
268
1196
384
1229
toleranceAngle
toleranceAngle
0
360
10.7
0.1
1
NIL
HORIZONTAL

SLIDER
271
1281
389
1314
Vision
Vision
1
10
2
1
1
NIL
HORIZONTAL

INPUTBOX
908
1317
1063
1377
periodeUpdateStockConstant
300
1
0
Number

INPUTBOX
909
1383
1064
1443
periodeRecalculProd
57
1
0
Number

SLIDER
404
1148
558
1181
pondereInfoServiceVide
pondereInfoServiceVide
0
50
3
1
1
NIL
HORIZONTAL

SLIDER
883
1126
1013
1159
PeriodeRefresh
PeriodeRefresh
1
500
14
1
1
NIL
HORIZONTAL

TEXTBOX
308
1094
458
1112
Congestion
11
0.0
1

TEXTBOX
447
1090
597
1108
Chemin
11
0.0
1

TEXTBOX
419
1181
569
1199
Stock
11
0.0
1

TEXTBOX
291
1347
441
1365
Individu\n
11
0.0
1

SLIDER
410
1306
592
1339
periodeRecalculProd
periodeRecalculProd
0
1200
57
1
1
ticks
HORIZONTAL

BUTTON
884
1162
1067
1195
NIL
placerBoulangerie
T
1
T
OBSERVER
NIL
NIL
NIL
NIL

SLIDER
408
1338
634
1371
PeriodeTraitementFileAttente
PeriodeTraitementFileAttente
0
120
1
1
1
ticks
HORIZONTAL

SLIDER
654
1159
843
1192
MaxUnitStockDelivreParDemande
MaxUnitStockDelivreParDemande
1
5
5
1
1
NIL
HORIZONTAL

SWITCH
289
1454
432
1487
hideIndividuals?
hideIndividuals?
1
1
-1000

SLIDER
659
1277
831
1310
prix_unite
prix_unite
0
10
1
1
1
NIL
HORIZONTAL

SLIDER
661
1316
833
1349
prix_vente
prix_vente
0
10
5
1
1
NIL
HORIZONTAL

SLIDER
664
1354
836
1387
perte_insatisfait
perte_insatisfait
0
10
5
1
1
NIL
HORIZONTAL

SLIDER
662
1393
834
1426
perte_invendu
perte_invendu
0
10
5
1
1
NIL
HORIZONTAL

SLIDER
658
1238
831
1271
valorisation_client_potentiel
valorisation_client_potentiel
0
10
4
1
1
NIL
HORIZONTAL

SLIDER
656
1198
828
1231
loyer
loyer
0
100
40
1
1
NIL
HORIZONTAL

SLIDER
283
1532
455
1565
seuil_affichage
seuil_affichage
0
5
2
0.1
1
NIL
HORIZONTAL

SLIDER
289
1495
461
1528
dist_services
dist_services
1
100
10
1
1
NIL
HORIZONTAL

MONITOR
1019
524
1097
569
Score Jeu 2
perfs_myservice
1
1
11

BUTTON
847
213
985
246
ouvrir boulangerie
set creer-on-fermer-off true
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
848
252
984
285
fermer boulangerie
set creer-on-fermer-off false
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
875
394
988
448
Jouer au Jeu 1
set Creer_Ma_Boulangerie false
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
915
523
1011
574
Jouer au Jeu 2
if jeu1OK? [set Creer_Ma_Boulangerie true]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
988
213
1084
284
pause
set pause? not pause?
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

SWITCH
887
1235
1068
1268
Creer_Ma_Boulangerie
Creer_Ma_Boulangerie
1
1
-1000

SWITCH
889
1272
1062
1305
temps-on-density-off
temps-on-density-off
1
1
-1000

SWITCH
884
1199
987
1232
pause?
pause?
1
1
-1000

MONITOR
1022
371
1100
416
Equité
graphe_temps2
1
1
11

MONITOR
1024
325
1100
370
Efficacité
graphe_temps
1
1
11

MONITOR
1021
418
1100
463
Richesse
sum [argent] of services
1
1
11

BUTTON
858
307
954
340
voir temps
if not temps-on-density-off [showTemps]\nset temps-on-density-off true\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

BUTTON
858
341
954
374
voir densité
if temps-on-density-off [showDensity]\nset temps-on-density-off false\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL

TEXTBOX
830
466
1139
516
Pour pouvoir Jouer au Jeu 2, il faut d'abord gagner au Jeu 1!
20
0.0
1

MONITOR
838
526
910
571
Jeu 1 OK ?
jeu1OK?
17
1
11

SLIDER
417
1271
589
1304
argent_initial
argent_initial
1
1000
116
1
1
NIL
HORIZONTAL

@#$#@#$#@
EN FRANCAIS
------------

WHAT IS IT?
-----------

Accessim est une simulation à base d’agent qui est censé représenter de manière pédagogique et interactive les déplacements et accessibilités dans une ville. Cette simulation est programmée sur la plateforme gratuite NetLogo qui permet de réaliser des simulations sans être nécessairement un développeur confirmé.
Cette simulation est composée de plusieurs agents qui sont : des individus, des quartiers, des services et le réseau. 
Les agents individus sont régis par des règles de comportements qui leur permettent de se déplacer sur le réseau, de rentrer et de consommer dans les services. 
Les agents services sont placés sur le réseau et offre des services aux individus. Ces services gèrent un stock et peuvent fermer lorsqu’il arrive à échéance.
Pour le « décideur en herbe » il est possible de visualiser un certain nombre d’informations qui va influencer le placement des services : l’accessibilité par quartier, le taux de satisfaction des individus, la distance moyenne pour atteindre un service, les ventes totales, etc.
L’objectif du modèle étant d’être interactif l’utilisateur peut rajouter ou enlever des services afin de voir de manière dynamique les accessibilités qui évoluent dans les quartiers et dans la ville. Un certain nombre de paramètres peut également être modifié  via le tableau de bord de la modélisation : choix de la densité de population, quantité de services à implanter, vitesse des individus, etc.



HOW IT WORKS
------------

1) Le réseau

Les individus se déplacent exclusivement sur ce réseau jusqu’à ce qu’ils trouvent un service, ou que leur durée de parcours soit écoulée.
Les services peuvent être déposés sur les nœuds du réseau par les utilisateurs.
Ce réseau est constitué de nœuds fixes reliés par des arcs non valués. NetLogo permet l’utilisation d’agent « link » pour modéliser les liens entre plusieurs agents nœuds. La localisation et les relations entre nœuds sont chargées depuis un fichier texte.

2) Les individus 

a) Apparition dans le modèle
Il existe deux types d’individus dans le modèle : suiveur et explorateur. Ils sont implémentés sous la forme d’un seul agent ayant une variable spécifiant le type d’individu.  
Les individus apparaissent dans le modèle en fonction des densités de population du quartier, on utilise une méthode de criblage : un quartier ayant une forte densité de population aura plus de chance à chaque tour de faire apparaître un individu dans la simulation qu’un quartier ayant une plus faible densité de population. 
b) Le cas de l’individu explorateur 
L’individu explorateur se déplace de manière aléatoire sur le du réseau. 
Lorsque l’individu passe sur un nœud, ce dernier est stocké dans une liste interne de l’agent individu. Lors de la rencontre d’un explorateur avec un service, un calcul de satisfaction est réalisé qui prend en compte le temps de parcours (inverse du temps mis par l’individu avant d’accéder au service) et le pourcentage d’appétit satisfait par le service. Cet indice de satisfaction est enregistré en entête de la liste de nœud ayant permis d’accéder au service. Quand l’individu explorateur rentre dans son quartier, il transmet cette liste à son quartier d’origine et disparaît. Cette liste sera ce qu’on appelle un « chemin » par la suite. Un autre individu réapparait immédiatement ailleurs dans cette simulation.
Un individu explorateur qui n’a pas trouvé de service sur son chemin ne transmet pas la liste de nœuds parcourus à son quartier.
c) Le cas de l’individu suiveur :
Lorsqu’il apparaît, cet individu lance un tirage probabiliste pour déterminer s’il va demander à trier les chemins existants en fonction de la satisfaction. Il récupère dans tous les cas le premier élément de la liste, qui n’est pas forcément le meilleur chemin, pour initialiser son futur chemin parcouru.  
Si le suiveur rencontre un autre service que celui pour lequel il est destiné, il enregistre l’information de la même manière que le ferait un individu explorateur : il rentre dans le service, calcule l’indice de satisfaction et rentre chez lui. Le quartier sera mis à jour avec l’ajout de ce nouveau chemin. Il n’y a aucun test dans ce modèle qui permette à l’individu de différencier un service qui a été déjà répertorié d’un autre service, ce qui engendre assez vite la création d’un certain nombre de chemins doublons dans la liste de chemins du quartier.
d) Schéma bilan de la logique individu :
Attention, les fonctions ne sont pas détaillées dans ce schéma et comportent des logiques autres que celles qui sont sous entendues dans le nom des fonctions. Il est donc impossible de comparer ce modèle avec le diagramme d’activité qui est présenté pour la nouvelle architecture de l’agent individu.

 
  


L’indice de satisfaction des individus est calculé en divisant le besoin par la part délivrée par le service. On multiplie ensuite le résultat de ce calcul par l’inverse de la distance parcourue. 
Par exemple, si le besoin de l’individu est de trois unités et que le service n’a plus que deux unités, alors le taux de satisfaction sera de deux tiers multiplié par l’inverse de la distance. Dans ce cas là l’individu rentrera chez lui pas totalement satisfait. 
Dans le cas où le service n’a plus de stock et ne peut rien lui délivrer alors il rentrera chez lui insatisfait. 
Dans l’évolution du modèle nous verrons comment l’individu sera autorisé à chercher un autre service lorsqu’il est insatisfait.
Le retour de chaque individu se fait en fonction d’une horloge interne (fixée de manière exogène par une variable sur le tableau de bord de la simulation) qui indique à partir de quel moment celui ci retourne à son domicile, qu’il ait trouvé un service ou non.

3) Agent Quartier

a) Apparition dans le modèle
Chaque quartier est un agent, incapable de bouger comme les individus, invisible aux yeux de l’utilisateur. C’est  pourtant lui qui est le cœur du modèle. Effectivement ils sont chargés de calculer le taux de « bonheur » des quartiers à partir des connaissances rapportées par les individus. C’est cette information que l’on affiche ensuite aux décideurs  chargés d’implémenter les futurs services. On utilise un camaïeu de rose pour représenter ce taux de « bonheur ».
b) Enregistrement des connaissances
Inspiré de l’architecture informatique nommé « blackboard  » (tableau noir) chaque individu qui a rencontré un service ajoute son information à un tableau noir géré par le quartier (implémenté par une liste).
Dans notre modèle la problématique sous jacente au tableau noir quartier est de trouver le chemin vers un service qui a le plus grand taux de satisfaction possible. Les individus, lorsqu’ils rencontrent un nouveau service, apportent une réponse partielle à cette problématique. Dès lors,  passé un certain nombre d’itération, ce tableau noir va converger vers une solution qui présentera effectivement le « meilleur chemin ». 
A chaque départ d’un individu suiveur le tableau noir est trié et l’individu prend le chemin ayant le meilleur indice de satisfaction.
Nous verrons dans la suite du rapport comment étendre ce modèle pour gérer à la fois des cas plus complexes d’enregistrement de chemin et  la possibilité pour les individus de choisir  le chemin autrement qu’en prenant le meilleur en satisfaction.
c). Affichage du taux de bonheur
Cet indice bonheur est calculé en réalisant une moyenne évolutive des taux de satisfaction ramenés par les individus. On décide dans cette moyenne de donner un poids plus important au passé récent, le poids des taux de satisfaction des individus récemment arrivés étant plus élevé que les poids des taux de satisfaction passés. C’est une manière de rendre l’évolution du taux de bonheur plus dynamique.

4) Les agents services

 Gestion des stocks , basée sur une approximation ici car cette équation est décrite avec l’utilisation d’une loi normale, ou une loi poisson, calculée sur la base d’une fréquentation du service pour l’ensemble des jours précédents. Notre modèle implémente seulement une équation qui se base sur les dernières ventes.
Cette nouvelle gestion des stocks se fait sur la base de plusieurs paramètres fixés de manière exogène via le tableau de bord de la simulation :
•	La période de recalcul des stocks
•	Le nombre de client insatisfait
•	Le nombre de client satisfait
•	Le prix d’achat d’une unité de stock
•	Le prix de vente d’une unité à un individu
•	La perte du à un invendu
•	La perte du à un client insatisfait
•	Le loyer à payer à chaque pas de temps
Tous les paramètres n’influent pas sur le calcul du nouveau stock, seul le nombre de clients satisfaits, la perte due à un invendu, la perte due à un client insatisfait rentre en compte dans l’équation.
La fermeture du service intervient lorsque celui-ci n’a plus la capacité à acheter des stocks ou qu’il n’est plus en mesure de payer le loyer.



HOW TO USE IT
-------------
This section could explain how to use the model, including a description of each of the items in the interface tab.

L'utilisateur commence par choisir la forme du réseau viaire de la ville : il peut choisir parmi plusieurs réseaux prédéfinis : réseau radioconcentrique, en grille régulière, ou bien choisir le réseau de rues du 12ème arrondissement parisien, etc.. (paramètre ...). Il choisit ensuite la forme de la répartition de l'habitat sur ce réseau viaire. Là encore, on peut choisir parmi plusieurs configurations pré-établies : 

THINGS TO NOTICE
----------------
I. Hypothèses du modèle 
Dans le modèle Accessim initial, le chemin sélectionné par un individu à l’initialisation est celui qui possède le meilleur indice de satisfaction.
Cet indice est calculé en tenant compte de l’inverse de la distance au service et du pourcentage de satisfaction de l’individu.
Rapidement s’est posée la question d’un autre moyen pour gérer le choix du chemin. Nous somme parti de l’hypothèse qu’un individu ne choisi pas seulement d’accéder à un service parce qu’il est le plus prés, mais aussi parce que ce service est plus ou moins fréquenté.
I. Description de l’algorithme implémenté 
Lors de l’initialisation d’un individu, le quartier demande à récupérer dans sa base de chemin connue les identifiants de deux chemins : le premier doit être celui qui possède la fréquence la plus élevée de parcours, le deuxième doit être celui qui a le temps de parcours le plus court.
L’algorithme a été proposé par un de mes tuteurs de stage, je n’ai fait que l’implémenter.  
1. Initialisation des variables 
On considère un chemin p qui est le plus probable, et un chemin e qui est le plus efficace.
Le chemin p est le maximum fréquence,
Le chemin e le plus efficace est celui qui possède le temps minimum de parcours,
Alpha est un paramètre exogène pour modifier le poids de la fréquence dans le calcul du rapport final
1. Calcul des rapports 
Soit n le résultat du rapport entre la fréquence du chemin p et la fréquence du chemin e
Soit d le résultat du rapport entre la distance du chemin p et la distance du chemin e
Si d > alpha * n alors on prend le chemin le plus court, c'est-à-dire le chemin p
Si d < alpha * n alors on prend le chemin le plus fréquenté, c'est-à-dire le chemin e
2. Exemple concret 
Le chemin n à une fréquence de 20, et un temps de parcours de 20
Le chemin p à une fréquence de 1 et un temps de parcours de 19
Si alpha vaut 1 alors n = 10 et d = 1, 05, d est donc inférieur à n, c’est le chemin le plus fréquenté qui est choisi par l’individu.
La différence de temps de parcours ne suffit pas à effacer la différence de fréquentation. Autrement dit, pour contrebalancer la différence de fréquence dans le choix du chemin, il faudrait que la différence de temps de parcours soit beaucoup plus importante. 
En modifiant le paramètre alpha, le modélisateur décide de donner un poids plus ou moins important (entre 0 et 1 sur le tableau de bord) au rapport de fréquence des chemins dans le résultat final.
Cette ébauche d’algorithme de comportement est une tentative pour simuler le fait que les individus ont tendance à suivre un même chemin si celui-ci est très fréquenté, et cela même si un autre chemin, beaucoup plus court, existe. 


THINGS TO TRY
-------------
This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.


EXTENDING THE MODEL
-------------------
This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.


NETLOGO FEATURES
----------------
This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.


RELATED MODELS
--------------
This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.


IN ENGLISH
--------------

WHAT IS IT?
-----------
This section could give a general understanding of what the model is trying to show or explain.

QU'est ce que l'équité en termes d'accessibilité ? Quelles localisations des ressources apparaissent optimales ou au contraire conduisent à l'exclusion des territoires ?

L'objectif est de montrer que la répartition des services ou équipements est le plus souvent inégale, et que leur accès est différent selon les territoires, qui peuvent être bien desservis ou exclus, créant ainsi des inégalités de satisfaction entre les habitants. 
L'objectif de ce modèle est d'illustrer ces idées dans un cadre très simplifié : l'accès dans un contexte urbain à un commerce de proximité : des boulangeries. Les individus à la recherche de ce service de proximité sont ici des enfants. 


HOW IT WORKS
------------
This section could explain what rules the agents use to create the overall behavior of the model.

Le modèle simule des habitants en ville, ici des enfants. Les équipements sont des boulangeries. Il y a donc deux types d'agents interragissant dans l'environnement urbain : des agents enfants, et des agents boulangeries. Le comportement de chaque type d'agent est régi et contraint par quelques règles très simples : 

- les enfants parcourent les rues pour trouver une boulangerie et consommer des pains au chocolat. Ils ont pour cela un temps limité (paramètre ...)

- les boulangeries doivent adapter leurs stocks à la demande qui dépend de la clientèle potentielle et du contexte contexte concurrentiel. L'ajustement de leur stock est régi par les paramètres ... et ... . Si il s'écoule plus de ... itérations sans qu'elle ait renouvellé son stock, une boulangerie ferme, et l'agent est supprimé. 


HOW TO USE IT
-------------
This section could explain how to use the model, including a description of each of the items in the interface tab.

L'utilisateur commence par choisir la forme du réseau viaire de la ville : il peut choisir parmi plusieurs réseaux prédéfinis : réseau radioconcentrique, en grille régulière, ou bien choisir le réseau de rues du 12ème arrondissement parisien, etc.. (paramètre ...). Il choisit ensuite la forme de la répartition de l'habitat sur ce réseau viaire. Là encore, on peut choisir parmi plusieurs configurations pré-établies : 

THINGS TO NOTICE
----------------
This section could give some ideas of things for the user to notice while running the model.


THINGS TO TRY
-------------
This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.


EXTENDING THE MODEL
-------------------
This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.


NETLOGO FEATURES
----------------
This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.


RELATED MODELS
--------------
This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

CREDITS AND REFERENCES
----------------------
This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
true
0
Polygon -7500403 true true 180 0 164 21 144 39 135 60 132 74 106 87 84 97 63 115 50 141 50 165 60 225 150 300 165 300 225 300 225 0 180 0
Circle -16777216 true false 180 30 90
Circle -16777216 true false 180 180 90
Polygon -16777216 true false 80 138 78 168 135 166 135 91 105 106 96 111 89 120
Circle -7500403 true true 195 195 58
Circle -7500403 true true 195 47 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

factory
false
0
Rectangle -7500403 true true 76 194 285 270
Rectangle -7500403 true true 36 95 59 231
Rectangle -16777216 true false 90 210 270 240
Line -7500403 true 90 195 90 255
Line -7500403 true 120 195 120 255
Line -7500403 true 150 195 150 240
Line -7500403 true 180 195 180 255
Line -7500403 true 210 210 210 240
Line -7500403 true 240 210 240 240
Line -7500403 true 90 225 270 225
Circle -1 true false 37 73 32
Circle -1 true false 55 38 54
Circle -1 true false 96 21 42
Circle -1 true false 105 40 32
Circle -1 true false 129 19 42
Rectangle -7500403 true true 14 228 78 270

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

house_transparente
false
0
Rectangle -7500403 true true 45 240 90 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120
Rectangle -7500403 true true 210 135 255 285
Rectangle -7500403 true true 45 135 90 285
Rectangle -7500403 true true 90 135 210 180

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

letter sealed
false
0
Rectangle -7500403 true true 30 90 270 225
Rectangle -16777216 false false 30 90 270 225
Line -16777216 false 270 105 150 180
Line -16777216 false 30 105 150 180
Line -16777216 false 270 225 181 161
Line -16777216 false 30 225 119 161

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

rabbit
false
0
Polygon -7500403 true true 61 150 76 180 91 195 103 214 91 240 76 255 61 270 76 270 106 255 132 209 151 210 181 210 211 240 196 255 181 255 166 247 151 255 166 270 211 270 241 255 240 210 270 225 285 165 256 135 226 105 166 90 91 105
Polygon -7500403 true true 75 164 94 104 70 82 45 89 19 104 4 149 19 164 37 162 59 153
Polygon -7500403 true true 64 98 96 87 138 26 130 15 97 36 54 86
Polygon -7500403 true true 49 89 57 47 78 4 89 20 70 88
Circle -16777216 true false 37 103 16
Line -16777216 false 44 150 104 150
Line -16777216 false 39 158 84 175
Line -16777216 false 29 159 57 195
Polygon -5825686 true false 0 150 15 165 15 150
Polygon -5825686 true false 76 90 97 47 130 32
Line -16777216 false 180 210 165 180
Line -16777216 false 165 180 180 165
Line -16777216 false 180 165 225 165
Line -16777216 false 180 210 210 240

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 4.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
