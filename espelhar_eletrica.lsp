;;; =========================================================================
;;; ESPELHAR_ELETRICA.LSP - v17.0
;;; Espelhamento Inteligente para Projetos Elétricos no AutoCAD
;;; =========================================================================
;;; 1. ESPELHO HORIZONTAL (Cima / Baixo) - 100% Preservado:
;;;    - Desvira blocos de interruptores ('S') e tags de luz no próprio centro
;;;    - Blocos de fiação (pentes de retornos/fase/neutro/terra): espelhados
;;;      pelo seu PRÓPRIO ponto de inserção (na marcação/linha guia).
;;;      Deslocamento ZERO para fora da marcação! Ficam apoiados apontando para CIMA!
;;;    - Textos da fiação (layer *FIA*): bitola # desce 0.4 e circuitos sobem 0.4
;;;    - Tomadas e demais layers 100% protegidos
;;; 2. ESPELHO VERTICAL (Esquerda / Direita) - Ordem 1, 2, 3... e Distância 0.1250:
;;;    - Sentido correto: blocos reespelhados com ganchos para a ESQUERDA
;;;    - Ordem correta rigorosamente mantida: o circuito que está na posição 1
;;;      continua na posição 1, o da 2 na 2, e assim sucessivamente (texto + bloco)
;;;    - Espaçamento de 0.1250 milimetricamente mantido entre blocos adjacentes!
;;;    - Textos perfeitamente centralizados acima de cada bloco respectivo
;;;    - Desvira interruptores ('S') e tags de iluminação no próprio centro
;;; =========================================================================

(vl-load-com)

(setvar "MIRRTEXT" 0)

(setq *ELET_FIA_OFFSET* 0.4)
(setq *ELET_FIA_GAP* 0.1250) ; Distância exata entre blocos de fiação

;; Padrões de blocos que devem ficar sempre em pé (interruptores, tags de luz)
(if (not *ELET_UPRIGHT_BLOCKS*)
  (setq *ELET_UPRIGHT_BLOCKS* 
    '("*INT*" "*SW*" "*SWITCH*" "*ILUM*" "*LUZ*" "*TAG*" "*LUM*" "*S_*")
  )
)

;;; -------------------------------------------------------------------------
;;; Tratador de erros para restaurar variáveis de sistema
;;; -------------------------------------------------------------------------
(defun elet:error-handler (msg)
  (setvar "CMDECHO" 1)
  (setvar "MIRRTEXT" 0)
  (if (and msg (not (wcmatch (strcase msg) "*FUNCTION CANCELLED*,*QUIT / EXIT ABORT*,*CANCELAR*")))
    (princ (strcat "\n[EE - Erro]: " msg))
  )
  (princ)
)

;;; -------------------------------------------------------------------------
;;; Obter nome efetivo do bloco
;;; -------------------------------------------------------------------------
(defun elet:get-block-name (obj / name)
  (if (vlax-property-available-p obj 'EffectiveName)
    (strcase (vla-get-EffectiveName obj))
    (strcase (vla-get-Name obj))
  )
)

;;; -------------------------------------------------------------------------
;;; Identifica se o bloco é símbolo a desvirar no lugar (Interruptores 'S', Tags)
;;; NUNCA mexe em tomadas nem fiação aqui
;;; -------------------------------------------------------------------------
(defun elet:should-flip-symbol (obj / name lay isMatch)
  (setq name    (elet:get-block-name obj)
        lay     (strcase (vla-get-Layer obj))
        isMatch nil)
  (foreach pat *ELET_UPRIGHT_BLOCKS*
    (if (wcmatch name (strcase pat))
      (setq isMatch T)
    )
  )
  (and
    ;; Não é tomada nem equipamento
    (not (wcmatch name "*TOM*,*TMD*,*PLUG*,*SOCKET*"))
    (not (wcmatch lay "*EQUIP*,*TOM*"))
    ;; Não é fiação (fiação é tratada com seus textos)
    (not (wcmatch name "*FIA*,*PENTE*,*TRACO*,*CONDUTOR*"))
    (not (wcmatch lay "*FIA*"))
    ;; É interruptor, tag ou iluminação
    isMatch
  )
)

;;; -------------------------------------------------------------------------
;;; Desvira o bloco NO PRÓPRIO LUGAR (espelha em torno do seu centro geométrico)
;;; -------------------------------------------------------------------------
(defun elet:flip-block-in-place (ent isHoriz / obj minPt maxPt bMin bMax center p2)
  (setq obj (vlax-ename->vla-object ent))
  (vl-catch-all-apply
    '(lambda ()
       (vla-GetBoundingBox obj 'minPt 'maxPt)
       (setq bMin (vlax-safearray->list minPt)
             bMax (vlax-safearray->list maxPt))
       (setq center (list (/ (+ (car bMin) (car bMax)) 2.0)
                          (/ (+ (cadr bMin) (cadr bMax)) 2.0)
                          0.0))
       (if isHoriz
         (setq p2 (list (+ (car center) 1.0) (cadr center) 0.0))
         (setq p2 (list (car center) (+ (cadr center) 1.0) 0.0))
       )
       (command "_.MIRROR" ent "" "_non" center "_non" p2 "_Y")
     )
  )
)

;;; -------------------------------------------------------------------------
;;; Ponto de alinhamento de texto (respeita justificação Middle center, etc.)
;;; -------------------------------------------------------------------------
(defun elet:get-text-pt (elist / typ h72 h73)
  (setq typ (cdr (assoc 0 elist)))
  (cond
    ((= typ "TEXT")
     (setq h72 (cdr (assoc 72 elist))
           h73 (cdr (assoc 73 elist)))
     (if (and (or (null h72) (= h72 0))
              (or (null h73) (= h73 0)))
       (cdr (assoc 10 elist))
       (cdr (assoc 11 elist))
     )
    )
    ((= typ "MTEXT")
     (cdr (assoc 10 elist))
    )
    (T (cdr (assoc 10 elist)))
  )
)

;;; -------------------------------------------------------------------------
;;; Deslocamento da Fiação: Apenas na camada *FIA* (# desce 0.4, circuitos sobem 0.4)
;;; -------------------------------------------------------------------------
(defun elet:deslocar-fiacao-layer-only (ss / i ent elist typ tStr lay countHash countCirc)
  (setq countHash 0
        countCirc 0
        i 0)
  (setvar "CMDECHO" 0)
  (while (< i (sslength ss))
    (setq ent   (ssname ss i)
          elist (entget ent)
          typ   (cdr (assoc 0 elist)))
    
    (if (or (= typ "TEXT") (= typ "MTEXT"))
      (progn
        (setq lay  (strcase (cdr (assoc 8 elist)))
              tStr (cdr (assoc 1 elist)))
        
        ;; Move APENAS se estiver na camada de fiação
        (if (wcmatch lay "*FIA*")
          (progn
            ;; Se contém '#': desce 0.4 (-0.4)
            (if (vl-string-search "#" tStr)
              (progn
                (command "_.MOVE" ent "" "_non" '(0.0 0.0 0.0) "_non" (list 0.0 (- *ELET_FIA_OFFSET*) 0.0))
                (setq countHash (1+ countHash))
              )
              ;; Se não contém '#' (circuitos): sobe 0.4 (+0.4)
              (progn
                (command "_.MOVE" ent "" "_non" '(0.0 0.0 0.0) "_non" (list 0.0 *ELET_FIA_OFFSET* 0.0))
                (setq countCirc (1+ countCirc))
              )
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (setvar "CMDECHO" 1)
  (if (or (> countHash 0) (> countCirc 0))
    (princ (strcat "\n>>> [FIAÇÃO]: " (itoa countHash) " bitola(s) (#) descida(s) 0.4 e "
                   (itoa countCirc) " circuito(s) subido(s) 0.4!"))
  )
)

;;; -------------------------------------------------------------------------
;;; Corrige traços e blocos de fiação no espelho horizontal:
;;; Cada bloco é espelhado pela linha horizontal do seu PRÓPRIO ponto de inserção!
;;; Isso garante DESLOCAMENTO ZERO para fora da marcação:
;;; O bloco fica apoiado exatamente na linha da marcação, apontando para CIMA!
;;; -------------------------------------------------------------------------
(defun elet:corrigir-tracos-horizontal-up (ss / i ent elist typ lay pt p1 p2 x1 x2 y1 y2 yTop yBot)
  (setvar "CMDECHO" 0)
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent   (ssname ss i)
          elist (entget ent)
          typ   (cdr (assoc 0 elist))
          lay   (strcase (cdr (assoc 8 elist))))
    
    (if (wcmatch lay "*FIA*")
      (cond
        ;; 1. Bloco de fiação (INSERT):
        ;; Espelha pelo seu próprio ponto de inserção (que está sobre a marcação)
        ((= typ "INSERT")
         (setq pt (cdr (assoc 10 elist)))
         (if pt
           (progn
             (setq p1 (list (car pt) (cadr pt) 0.0)
                   p2 (list (+ (car pt) 1.0) (cadr pt) 0.0))
             (command "_.MIRROR" ent "" "_non" p1 "_non" p2 "_Y")
           )
         )
        )
        
        ;; 2. Traço solto em LINHA (se houver linhas verticais soltas de fiação):
        ;; Apenas linhas verticais de comprimento curto (0.03m a 0.40m)
        ((= typ "LINE")
         (setq p1 (cdr (assoc 10 elist))
               p2 (cdr (assoc 11 elist))
               x1 (car p1)
               y1 (cadr p1)
               x2 (car p2)
               y2 (cadr p2))
         (if (and (<= (abs (- x1 x2)) 0.05)
                  (<= (distance p1 p2) 0.40)
                  (>= (distance p1 p2) 0.03))
           (progn
             (setq yTop (max y1 y2)
                   yBot (min y1 y2))
             (setq elist (subst (cons 10 (list x1 yTop (caddr p1)))
                                (assoc 10 elist)
                                elist))
             (setq elist (subst (cons 11 (list x2 (+ yTop (- yTop yBot)) (caddr p2)))
                                (assoc 11 elist)
                                elist))
             (entmod elist)
           )
         )
        )
      )
    )
    (setq i (1+ i))
  )
  (setvar "CMDECHO" 1)
)

;;; -------------------------------------------------------------------------
;;; Reespelha e distribui fiação no espelho vertical:
;;; 1. Reespelha cada bloco/traço condutor no próprio centro (ganchos para a ESQUERDA)
;;; 2. Distribui os blocos com a distância exata de 0.1250 entre blocos adjacentes!
;;; 3. Centraliza cada texto de circuito exatamente sobre o seu bloco respectivo
;;; -------------------------------------------------------------------------
(defun elet:corrigir-fiacao-espelho-vertical (ss / i ent elist typ lay tStr pt h
                                                  circTexts wireEnts newWireEnts
                                                  obj minPt maxPt bMin bMax bCenter diag
                                                  lastE nextE wItem bestT bestDist dist tItem tEnt tPt tX tY
                                                  colMap groups colItem foundG newGroups
                                                  grp k sortedCols orderCols firstMinX xCurr
                                                  tEntCol wListCol currTX colMinX colMaxX
                                                  wWidth wCurrC wNewC dxW dxT countFixed)
  (setvar "CMDECHO" 0)
  
  ;; 1. Coleta textos de circuitos (camada FIACAO, sem '#', altura <= 0.15)
  (setq circTexts '()
        i 0)
  (while (< i (sslength ss))
    (setq ent   (ssname ss i)
          elist (entget ent)
          typ   (cdr (assoc 0 elist))
          lay   (strcase (cdr (assoc 8 elist))))
    (if (and (wcmatch lay "*FIA*")
             (or (= typ "TEXT") (= typ "MTEXT")))
      (progn
        (setq tStr (cdr (assoc 1 elist)))
        (if (not (vl-string-search "#" tStr))
          (progn
            (setq h (cdr (assoc 40 elist)))
            (if (null h) (setq h 0.08))
            (if (<= h 0.15)
              (setq circTexts (cons (list ent (elet:get-text-pt elist)) circTexts))
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  
  ;; 2. Coleta entidades condutoras de fiação (blocos INSERT ou linhas curtas, diagonal <= 0.60m)
  (setq wireEnts '()
        i 0)
  (while (< i (sslength ss))
    (setq ent   (ssname ss i)
          elist (entget ent)
          typ   (cdr (assoc 0 elist))
          lay   (strcase (cdr (assoc 8 elist))))
    (if (and (wcmatch lay "*FIA*")
             (/= typ "TEXT")
             (/= typ "MTEXT"))
      (vl-catch-all-apply
        '(lambda ()
           (setq obj (vlax-ename->vla-object ent))
           (vla-GetBoundingBox obj 'minPt 'maxPt)
           (setq bMin (vlax-safearray->list minPt)
                 bMax (vlax-safearray->list maxPt)
                 diag (distance bMin bMax))
           ;; Apenas blocos e traços condutores pequenos (nunca conduíte nem linha guia)
           (if (and (<= diag 0.60) (>= diag 0.02))
             (setq bCenter (list (/ (+ (car bMin) (car bMax)) 2.0)
                                 (/ (+ (cadr bMin) (cadr bMax)) 2.0)
                                 0.0)
                   wireEnts (cons (list ent bCenter) wireEnts))
           )
         )
      )
    )
    (setq i (1+ i))
  )
  
  ;; 3. REESPELHAR: Espelha cada entidade de fiação condutora em torno do seu centro X
  ;;    Assim os ganchos passam a apontar para a ESQUERDA (exatamente como na Imagem 2)!
  ;;    Captura os novos handles de entidade gerados pelo espelho do AutoCAD.
  (setq newWireEnts '())
  (foreach wItem wireEnts
    (setq ent     (car wItem)
          bCenter (cadr wItem)
          lastE   (entlast))
    (command "_.MIRROR" ent "" "_non" bCenter "_non" (list (car bCenter) (+ (cadr bCenter) 1.0) 0.0) "_Y")
    (setq nextE (entnext lastE))
    (if nextE
      (setq newWireEnts (cons (list nextE bCenter) newWireEnts))
      (setq newWireEnts (cons (list ent bCenter) newWireEnts))
    )
  )
  (setq wireEnts newWireEnts)
  
  ;; 4. Associa cada entidade condutora ao texto de circuito mais próximo
  (setq colMap '())
  (foreach tItem circTexts
    (setq tEnt (car tItem)
          tPt  (cadr tItem)
          colMap (cons (list (car tPt) (cadr tPt) tEnt '()) colMap))
  )
  
  (foreach wItem wireEnts
    (setq ent     (car wItem)
          bCenter (cadr wItem)
          bestT   nil
          bestDist 999.0)
    (foreach tItem circTexts
      (setq tPt  (cadr tItem)
            tX   (car tPt)
            tY   (cadr tPt))
      ;; Deve estar abaixo do texto em Y (entre 0.01m e 0.50m) e próximo em X (<= 0.35m)
      (if (and (<= (- tY (cadr bCenter)) 0.50)
               (>= (- tY (cadr bCenter)) 0.01)
               (<= (abs (- (car bCenter) tX)) 0.35))
        (progn
          (setq dist (abs (- (car bCenter) tX)))
          (if (< dist bestDist)
            (setq bestDist dist
                  bestT    (car tItem))
          )
        )
      )
    )
    (if bestT
      (setq colMap
        (mapcar '(lambda (col)
                   (if (eq (nth 2 col) bestT)
                     (list (nth 0 col) (nth 1 col) (nth 2 col) (cons ent (nth 3 col)))
                     col
                   ))
                colMap)
      )
    )
  )
  
  ;; 5. Agrupa colunas da MESMA marcação horizontal (mesmo Y +/- 0.05m e X próximo <= 0.60m)
  (setq groups '())
  (foreach colItem colMap
    (setq tX (nth 0 colItem)
          tY (nth 1 colItem)
          foundG nil
          newGroups '())
    (foreach grp groups
      (if (and (not foundG)
               (<= (abs (- tY (nth 1 (car grp)))) 0.05)
               (<= (abs (- tX (nth 0 (car grp)))) 0.60))
        (progn
          (setq foundG T)
          (setq newGroups (cons (cons colItem grp) newGroups))
        )
        (setq newGroups (cons grp newGroups))
      )
    )
    (if foundG
      (setq groups newGroups)
      (setq groups (cons (list colItem) groups))
    )
  )
  
  ;; 6. Para cada marcação com 2 ou mais circuitos:
  ;;    Aplica a distância exata de 0.1250 entre blocos adjacentes!
  ;;    Preserva rigorosamente a ordem: circuito 1 na 1ª posição (esquerda), 2 na 2ª, etc.
  (setq countFixed 0)
  (foreach grp groups
    (setq k (length grp))
    (if (>= k 2)
      (progn
        ;; Ordena as colunas da esquerda para a direita (como ficaram após o espelho)
        (setq sortedCols (vl-sort grp '(lambda (a b) (< (car a) (car b)))))
        
        ;; Ponto de início: borda mais à esquerda de todos os blocos do grupo
        (setq firstMinX nil)
        (foreach col grp
          (foreach wEnt (nth 3 col)
            (vl-catch-all-apply
              '(lambda ()
                 (setq obj (vlax-ename->vla-object wEnt))
                 (vla-GetBoundingBox obj 'minPt 'maxPt)
                 (setq bMin (vlax-safearray->list minPt))
                 (if (null firstMinX)
                   (setq firstMinX (car bMin))
                   (setq firstMinX (min firstMinX (car bMin)))
                 )
               )
            )
          )
        )
        (if (null firstMinX)
          (setq xCurr (nth 0 (car sortedCols)))
          (setq xCurr firstMinX)
        )
        
        ;; O espelho inverte a ordem física esquerda<->direita dos circuitos.
        ;; Para manter a ordem original (circuito da posição 1 na posição 1, 2 na 2, etc.):
        ;; invertemos a lista para distribuir da esquerda para a direita:
        (setq orderCols (reverse sortedCols))
        
        ;; Posiciona cada circuito da lista orderCols sequencialmente da esquerda para a direita
        ;; mantendo a distância exata de 0.1250 entre blocos adjacentes!
        (foreach col orderCols
          (setq tEntCol  (nth 2 col)
                wListCol (nth 3 col)
                tPt      (elet:get-text-pt (entget tEntCol))
                currTX   (car tPt))
          
          ;; Bounding box atual dos blocos desta coluna
          (setq colMinX nil
                colMaxX nil)
          (foreach wEnt wListCol
            (vl-catch-all-apply
              '(lambda ()
                 (setq obj (vlax-ename->vla-object wEnt))
                 (vla-GetBoundingBox obj 'minPt 'maxPt)
                 (setq bMin (vlax-safearray->list minPt)
                       bMax (vlax-safearray->list maxPt))
                 (if (null colMinX)
                   (setq colMinX (car bMin))
                   (setq colMinX (min colMinX (car bMin)))
                 )
                 (if (null colMaxX)
                   (setq colMaxX (car bMax))
                   (setq colMaxX (max colMaxX (car bMax)))
                 )
               )
            )
          )
          
          (if (and colMinX colMaxX)
            (progn
              (setq wWidth (- colMaxX colMinX)
                    wCurrC (/ (+ colMinX colMaxX) 2.0)
                    wNewC  (+ xCurr (/ wWidth 2.0))
                    dxW    (- wNewC wCurrC)
                    dxT    (- wNewC currTX))
              
              ;; Move os blocos da coluna
              (if (> (abs dxW) 0.0005)
                (foreach wEnt wListCol
                  (command "_.MOVE" wEnt "" "_non" '(0.0 0.0 0.0) "_non" (list dxW 0.0 0.0))
                )
              )
              ;; Centraliza o texto perfeitamente sobre os blocos
              (if (> (abs dxT) 0.0005)
                (command "_.MOVE" tEntCol "" "_non" '(0.0 0.0 0.0) "_non" (list dxT 0.0 0.0))
              )
              
              ;; Próximo bloco começa exatamente em (fim deste bloco + 0.1250)!
              (setq xCurr (+ xCurr wWidth *ELET_FIA_GAP*))
            )
            ;; Caso a coluna tenha apenas texto sem bloco
            (progn
              (setq dxT (- xCurr currTX))
              (if (> (abs dxT) 0.0005)
                (command "_.MOVE" tEntCol "" "_non" '(0.0 0.0 0.0) "_non" (list dxT 0.0 0.0))
              )
              (setq xCurr (+ xCurr 0.15 *ELET_FIA_GAP*))
            )
          )
        )
        (setq countFixed (1+ countFixed))
      )
    )
  )
  (setvar "CMDECHO" 1)
  (if (> countFixed 0)
    (princ (strcat "\n>>> [EE - Vertical]: " (itoa countFixed) " grupo(s) de fiação ajustados com distância exata de 0.1250!"))
  )
)

;;; -------------------------------------------------------------------------
;;; COMANDO: DF (ou DESVIRARFIA)
;;; -------------------------------------------------------------------------
(defun c:DF ( / ss)
  (princ "\n[DF] Selecione os elementos da fiação: ")
  (setq ss (ssget))
  (if ss
    (progn
      (setvar "CMDECHO" 0)
      (elet:corrigir-tracos-horizontal-up ss)
      (elet:deslocar-fiacao-layer-only ss)
      (setvar "CMDECHO" 1)
      (princ "\n>>> [DF] Fiação corrigida com sucesso!")
    )
    (princ "\nNenhum objeto selecionado.")
  )
  (princ)
)

(defun c:DESVIRARFIA () (c:DF))

;;; -------------------------------------------------------------------------
;;; COMANDO: DV (ou DESVIRAR)
;;; -------------------------------------------------------------------------
(defun c:DV ( / *error* opt isHoriz ss blkList i ent obj)
  (setq *error* elet:error-handler)
  (initget "Horizontal Vertical H V")
  (setq opt (getkword "\nQual espelho foi feito? [Horizontal(Cima/Baixo) / Vertical(Esq/Dir)] <Horizontal>: "))
  (setq isHoriz (or (null opt) (= opt "H") (= opt "Horizontal")))
  
  (princ (if isHoriz 
           "\n[DV - Horizontal] Selecione todo o ambiente espelhado: "
           "\n[DV - Vertical] Selecione todo o ambiente espelhado: "))
  (setq ss (ssget))
  (if ss
    (progn
      (setvar "CMDECHO" 0)
      
      ;; 1. Desvira símbolos (interruptores 'S', tags de luz)
      (setq blkList '()
            i 0)
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (if (= (cdr (assoc 0 (entget ent))) "INSERT")
          (progn
            (setq obj (vlax-ename->vla-object ent))
            (if (elet:should-flip-symbol obj)
              (setq blkList (cons ent blkList))
            )
          )
        )
        (setq i (1+ i))
      )
      (foreach bEnt blkList
        (elet:flip-block-in-place bEnt isHoriz)
      )
      
      ;; 2. Espelho Horizontal:
      (if isHoriz
        (progn
          (elet:corrigir-tracos-horizontal-up ss)
          (elet:deslocar-fiacao-layer-only ss)
        )
        ;; 3. Espelho Vertical: Reespelha traços e espaça com 0.1250 exatos
        (progn
          (elet:corrigir-fiacao-espelho-vertical ss)
        )
      )
      (setvar "CMDECHO" 1)
      (princ "\n>>> [DV] Correção concluída com sucesso!")
    )
    (princ "\nNenhum objeto selecionado.")
  )
  (princ)
)

(defun c:DESVIRAR () (c:DV))

;;; -------------------------------------------------------------------------
;;; COMANDO: EE (ou EMIRROR) - Inteligente
;;; -------------------------------------------------------------------------
(defun c:EE ( / *error* ss p1 p2 dx dy isHoriz delAns lastEnt newEnt ssNew blkList i ent obj)
  (setq *error* elet:error-handler)
  (setvar "CMDECHO" 0)
  (setvar "MIRRTEXT" 0)
  
  (princ "\n[EE] Selecione os elementos elétricos a espelhar: ")
  (setq ss (ssget))
  
  (if ss
    (progn
      (initget 1)
      (setq p1 (getpoint "\nPrimeiro ponto da linha de espelho: "))
      (initget 1)
      (setq p2 (getpoint p1 "\nSegundo ponto da linha de espelho: "))
      
      (setq dx (abs (- (car p2) (car p1)))
            dy (abs (- (cadr p2) (cadr p1))))
      (setq isHoriz (>= dx dy))
      
      (initget "Sim Nao S N Yes No Y")
      (setq delAns (getkword "\nApagar objetos de origem? [Sim/Nao] <Nao>: "))
      (if (null delAns) (setq delAns "Nao"))
      (if (or (= delAns "S") (= delAns "Sim") (= delAns "Y") (= delAns "Yes"))
        (setq delAns "_Y")
        (setq delAns "_N")
      )
      
      (setq lastEnt (entlast))
      (command "_.MIRROR" ss "" "_non" p1 "_non" p2 delAns)
      
      (setq ssNew (ssadd))
      (setq newEnt (entnext lastEnt))
      (while newEnt
        (ssadd newEnt ssNew)
        (setq newEnt (entnext newEnt))
      )
      (if (= (sslength ssNew) 0) (setq ssNew ss))
      
      ;; 1. Desvira símbolos no próprio centro (interruptores 'S', tags de luz)
      (setq blkList '()
            i 0)
      (while (< i (sslength ssNew))
        (setq ent (ssname ssNew i))
        (if (= (cdr (assoc 0 (entget ent))) "INSERT")
          (progn
            (setq obj (vlax-ename->vla-object ent))
            (if (elet:should-flip-symbol obj)
              (setq blkList (cons ent blkList))
            )
          )
        )
        (setq i (1+ i))
      )
      (foreach bEnt blkList
        (elet:flip-block-in-place bEnt isHoriz)
      )
      
      ;; 2. Espelho Horizontal:
      (if isHoriz
        (progn
          (princ "\n[EE] Espelho Horizontal: fiação apoiada na marcação e retornos para CIMA...")
          (elet:corrigir-tracos-horizontal-up ssNew)
          (elet:deslocar-fiacao-layer-only ssNew)
        )
        ;; 3. Espelho Vertical: Reespelha traços e aplica distância exata de 0.1250
        (progn
          (princ "\n[EE] Espelho Vertical: fiação ajustada com distância exata de 0.1250...")
          (elet:corrigir-fiacao-espelho-vertical ssNew)
        )
      )
      (princ "\n>>> [EE] Espelhamento concluído com sucesso!")
    )
    (princ "\nNenhum objeto selecionado.")
  )
  (setvar "CMDECHO" 1)
  (princ)
)

(defun c:EMIRROR () (c:EE))

;;; -------------------------------------------------------------------------
;;; COMANDO: TF (ou TROCARFIA) - Troca posição de dois textos de circuito
;;; -------------------------------------------------------------------------
(defun c:TF ( / ent1 ent2 elist1 elist2 pt1 pt2)
  (princ "\n[TF] Clique no primeiro texto de circuito (ex: 1): ")
  (setq ent1 (car (entsel)))
  (if (and ent1 (or (= (cdr (assoc 0 (entget ent1))) "TEXT")
                    (= (cdr (assoc 0 (entget ent1))) "MTEXT")))
    (progn
      (princ "\n[TF] Clique no segundo texto de circuito (ex: T2): ")
      (setq ent2 (car (entsel)))
      (if (and ent2 (or (= (cdr (assoc 0 (entget ent2))) "TEXT")
                        (= (cdr (assoc 0 (entget ent2))) "MTEXT")))
        (progn
          (setq elist1 (entget ent1)
                elist2 (entget ent2)
                pt1    (elet:get-text-pt elist1)
                pt2    (elet:get-text-pt elist2))
          (command "_.MOVE" ent1 "" "_non" pt1 "_non" pt2)
          (command "_.MOVE" ent2 "" "_non" pt2 "_non" pt1)
          (princ "\n>>> [TF] Textos trocados de posição com sucesso!")
        )
        (princ "\nSegundo objeto inválido ou não selecionado.")
      )
    )
    (princ "\nPrimeiro objeto inválido ou não selecionado.")
  )
  (princ)
)

(defun c:TROCARFIA () (c:TF))

;;; -------------------------------------------------------------------------
;;; COMANDO: CFE (ou CFGELET) - Configurar Novos Blocos Interativamente
;;; -------------------------------------------------------------------------
(defun c:CFE ( / ent obj name)
  (princ "\n[CFE] Selecione um bloco na tela (ex: interruptor) para manter sempre em pé: ")
  (setq ent (car (entsel)))
  (if (and ent (= (cdr (assoc 0 (entget ent))) "INSERT"))
    (progn
      (setq obj  (vlax-ename->vla-object ent)
            name (elet:get-block-name obj))
      (if (not (vl-position name *ELET_UPRIGHT_BLOCKS*))
        (progn
          (setq *ELET_UPRIGHT_BLOCKS* (cons name *ELET_UPRIGHT_BLOCKS*))
          (princ (strcat "\n>>> Bloco \"" name "\" adicionado à lista de símbolos protegidos!"))
        )
        (princ (strcat "\n>>> Bloco \"" name "\" já está na lista protegida."))
      )
    )
    (princ "\nNenhum bloco selecionado.")
  )
  (princ)
)

(defun c:CFGELET () (c:CFE))

;;; -------------------------------------------------------------------------
;;; COMANDO: INSP (ou INFO) - Inspeciona entidade na tela
;;; -------------------------------------------------------------------------
(defun c:INSP ( / ent elist typ lay obj name)
  (princ "\n[INSP] Clique em qualquer elemento para inspecionar: ")
  (setq ent (car (entsel)))
  (if ent
    (progn
      (setq elist (entget ent)
            typ   (cdr (assoc 0 elist))
            lay   (cdr (assoc 8 elist)))
      (princ (strcat "\n--- Tipo: " typ " | Layer: " lay))
      (if (= typ "INSERT")
        (progn
          (setq obj  (vlax-ename->vla-object ent)
                name (elet:get-block-name obj))
          (princ (strcat " | Bloco: " name))
        )
      )
      (if (or (= typ "TEXT") (= typ "MTEXT"))
        (princ (strcat " | Conteúdo: \"" (cdr (assoc 1 elist)) "\""))
      )
      (princ " ---")
    )
    (princ "\nNenhum objeto selecionado.")
  )
  (princ)
)

(defun c:INFO () (c:INSP))

;;; Mensagem de carregamento
(princ "\n========================================================")
(princ "\n  ESPELHAR_ELETRICA v17.0 CARREGADO COM SUCESSO!")
(princ "\n  - Ordem 1, 2, 3... e Distância 0.1250 entre blocos de fiação")
(princ "\n  - Circuitos reespelhados, ordenados e centralizados")
(princ "\n  - Horizontal preservado 100% conforme aprovado")
(princ "\n  COMANDOS: EE (Espelhar) | DV (Desvirar) | DF (Fiação) | TF (Trocar)")
(princ "\n========================================================\n")
(princ)
