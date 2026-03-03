# Fichier : app.R
# Objectif : Tableau de bord statistique ULTIME (Normalité, Outliers, Corrélations)
# Version : +++ démos (beaucoup), SANS boutons de download
# Ajout : définitions étendues FR + AR + nouvelles stats (P10/P90, trim, winsor, JB, NA%)
# Auteur : R Code Wizard (refactor by R Wizard + extensions by R Wizard)

library(shiny)
library(shinydashboard)
library(datasets)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(moments)
library(tidyr)

# NEW: plotly for interactive heatmap
library(plotly)

options(shiny.maxRequestSize = 30 * 1024^2)

# -------------------------------------------------------------------
# 1) DICTIONNAIRE STATISTIQUE (FR + AR, paragraphes)
# -------------------------------------------------------------------
# Fully expanded `stat_dict`:
# - Definitions (FR/AR) are enriched
# - Math fields are filled for EVERY key
# - Structure stays identical so you can call stat_dict[[key]]$desc$fr/ar and $math in your app

stat_dict <- list(
  "n" = list(
    desc = list(
      fr = "Le nombre d’observations valides (n) correspond au total de valeurs effectivement utilisées dans les calculs, après exclusion des valeurs manquantes (NA) et, selon les règles de nettoyage, d’éventuelles valeurs non finies (Inf/NaN). C’est une information essentielle car elle conditionne la stabilité et la précision des estimations : en général, plus n est grand, plus la moyenne, les quantiles, l’écart-type, les corrélations et les tests sont stables et reproductibles. Attention : n ne mesure pas la “qualité” (biais, erreurs de mesure, bruit), mais seulement la quantité de données disponibles. Illustration : une moyenne calculée sur n=8 peut changer fortement si une seule valeur est atypique, alors qu’avec n=800 l’effet d’une valeur isolée est souvent dilué.",
      ar = "يمثل عدد الملاحظات الصالحة (n) إجمالي القيم المستخدمة فعليًا في الحسابات بعد استبعاد القيم المفقودة (NA) وأحيانًا القيم غير المنتهية (Inf/NaN) حسب قواعد التنظيف. هذه قيمة محورية لأنها تؤثر على استقرار ودقة التقديرات: عادةً كلما زاد n أصبحت مقاييس مثل المتوسط والكوانتيلات والانحراف المعياري والارتباطات والاختبارات أكثر ثباتًا وقابلية للتكرار. تنبيه: n لا يقيس “جودة” البيانات (تحيز، أخطاء قياس، ضوضاء) بل يوضح كمية القيم المتاحة. مثال: متوسط محسوب على n=8 قد يتغير كثيرًا بسبب قيمة شاذة واحدة، بينما عند n=800 غالبًا يضعف تأثير القيمة المفردة."
    ),
    math = "\\[ n = \\sum_{i=1}^{N} \\mathbf{1}(x_i\\ \\text{valide}) \\]"
  ),
  
  "n_na" = list(
    desc = list(
      fr = "Le nombre de valeurs manquantes (n_na) indique combien d’observations sont absentes (NA). En descriptif, on calcule souvent les statistiques en ignorant les NA, mais n_na reste crucial pour juger la complétude et le risque de biais. Si les NA sont non aléatoires (plus fréquents dans un groupe, une période ou pour certaines valeurs), ils peuvent fausser la moyenne, les comparaisons et les modèles. En pratique, n_na aide à décider entre suppression, imputation, ou analyse du mécanisme de manque (MCAR/MAR/MNAR). Indice : comparer n_na entre groupes/temps permet de détecter une structure du manque.",
      ar = "يشير عدد القيم المفقودة (n_na) إلى عدد الملاحظات غير المتوفرة (NA). في الوصف الإحصائي غالبًا تُحسب المقاييس بتجاهل NA، لكن n_na مهم لتقييم اكتمال البيانات وخطر التحيز. إذا كانت القيم المفقودة غير عشوائية (مثلاً أكثر في مجموعة أو فترة زمنية أو عند قيم معينة) فقد تُشوّه المتوسطات والمقارنات والنماذج. عمليًا يساعد n_na في اختيار الإستراتيجية: حذف، تعويض، أو دراسة آلية الفقد (MCAR/MAR/MNAR). مؤشر مفيد: مقارنة n_na عبر الفئات أو الزمن لاكتشاف نمط للفقد."
    ),
    math = "\\[ n_{NA} = \\sum_{i=1}^{N} \\mathbf{1}(x_i\\ \\text{manquant}) \\]"
  ),
  
  "na_pct" = list(
    desc = list(
      fr = "Le pourcentage de valeurs manquantes (NA%) met n_na en contexte par rapport à la taille totale. Il permet de comparer rapidement la complétude entre variables ou entre groupes. Un NA% élevé réduit la puissance statistique (moins d’informations), augmente l’incertitude et peut rendre certaines comparaisons instables, surtout si les NA se concentrent dans des catégories spécifiques. Règle pratique : au-delà de quelques %, inspectez la structure du manque et envisagez imputation ou analyse de sensibilité. Exemple : 20 NA sur 100 (20%) est bien plus problématique que 20 NA sur 10 000 (0,2%).",
      ar = "تضع نسبة القيم المفقودة (NA%) قيمة n_na في سياقها مقارنةً بالحجم الكلي، وتسهّل مقارنة اكتمال البيانات بين المتغيرات أو بين المجموعات. ارتفاع NA% يقلل القوة الإحصائية (معلومات أقل) ويزيد عدم اليقين وقد يجعل المقارنات غير مستقرة، خصوصًا إذا تركزت القيم المفقودة في فئات محددة. قاعدة عملية: إذا تجاوزت النسبة بضعة بالمئة افحص نمط الفقد وفكّر في التعويض أو تحليل الحساسية. مثال: 20 قيمة مفقودة من 100 (20%) أخطر بكثير من 20 من 10,000 (0.2%)."
    ),
    math = "\\[ NA\\% = \\frac{n_{NA}}{N}\\times 100 \\]"
  ),
  
  "sum" = list(
    desc = list(
      fr = "La somme est le total des valeurs valides (hors NA). Elle est utile pour des variables additives (quantités, ventes, volumes) lorsqu’on cherche un cumul. Mais elle dépend fortement de n : comparer des sommes entre groupes de tailles différentes peut être trompeur. On la complète souvent par la moyenne, par une somme “par unité” ou par des taux. Si la distribution est très asymétrique, quelques grandes valeurs peuvent dominer la somme : les quantiles et la médiane aident alors à contextualiser.",
      ar = "المجموع هو إجمالي القيم الصالحة (باستثناء NA). يفيد للمتغيرات التجميعية مثل الكميات والمبيعات والأحجام عندما نريد قيمة تراكمية. لكنه يعتمد بقوة على n: مقارنة مجموعات بأحجام مختلفة قد تكون مضللة. لذلك يُستحسن دعمه بالمتوسط أو المجموع لكل وحدة أو بالمعدلات. وإذا كان التوزيع منحرفًا جدًا فقد تهيمن قيم كبيرة قليلة على المجموع، وهنا تساعد الكوانتيلات والوسيط في إعطاء سياق."
    ),
    math = "\\[ \\text{Somme} = \\sum_{i=1}^{n} x_i \\]"
  ),
  
  "mean" = list(
    desc = list(
      fr = "La moyenne arithmétique résume les données par une valeur centrale (somme / n). Elle est informative pour des distributions relativement symétriques, mais sensible aux valeurs extrêmes (outliers) qui peuvent la déplacer. Bonne pratique : l’interpréter avec la médiane, l’IQR (ou MAD) et un graphique (histogramme/boxplot). Exemple : pour des revenus, la moyenne peut être nettement au-dessus de la “valeur typique” (médiane) à cause de quelques très hauts revenus.",
      ar = "المتوسط الحسابي يلخص البيانات بقيمة مركزية (المجموع / n). يكون مفيدًا للتوزيعات شبه المتماثلة لكنه حساس للقيم المتطرفة التي قد تغيّره. ممارسة جيدة: تفسيره مع الوسيط وIQR (أو MAD) ومع رسم بياني (مدرج/صندوقي). مثال: في الدخل قد يكون المتوسط أعلى بكثير من القيمة “النموذجية” (الوسيط) بسبب قلة من القيم العالية."
    ),
    math = "\\[ \\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i \\]"
  ),
  
  "mean_trim" = list(
    desc = list(
      fr = "La moyenne tronquée (trim) calcule la moyenne après avoir retiré une proportion des plus petites et des plus grandes valeurs (ex. 10% à chaque extrémité). Elle réduit l’influence des outliers tout en restant proche de l’idée de moyenne. C’est un compromis utile entre moyenne (sensible) et médiane (très robuste). Attention : le taux de troncature doit être documenté car il change le résultat.",
      ar = "المتوسط المُشذّب (Trimmed mean) يحسب المتوسط بعد حذف نسبة من أصغر وأكبر القيم (مثلاً 10% من كل طرف). يقلل تأثير القيم الشاذة مع الحفاظ على فكرة المتوسط. يعد حلًا وسطًا بين المتوسط (حساس) والوسيط (مقاوم جدًا). تنبيه: يجب توثيق نسبة التشذيب لأنها تؤثر على النتيجة."
    ),
    math = "\\[ \\bar{x}_{\\text{trim}} = \\frac{1}{n-2k}\\sum_{i=k+1}^{n-k} x_{(i)},\\quad k=\\lfloor \\alpha n \\rfloor \\]"
  ),
  
  "winsor_mean" = list(
    desc = list(
      fr = "La moyenne winsorisée remplace (au lieu de supprimer) les valeurs extrêmes par des valeurs seuils (par ex. quantiles 10% et 90%), puis calcule la moyenne. Elle limite l’impact des extrêmes tout en conservant n constant (on ne perd pas d’observations). Utile si l’on veut stabiliser la moyenne sans حذف بيانات. À utiliser de manière cohérente (mêmes seuils) et de façon transparente.",
      ar = "المتوسط الوينسوري (Winsorized mean) يستبدل القيم المتطرفة بقيم حدّية (مثل كوانتيل 10% و90%) ثم يحسب المتوسط. يحد من تأثير الأطراف مع الحفاظ على n ثابتًا (لا نحذف ملاحظات). مفيد لتثبيت المتوسط دون فقدان بيانات. يجب استخدام نفس الحدود عند المقارنة وبشفافية."
    ),
    math = "\\[ x_i^{\\text{win}}=\\min\\big(\\max(x_i,Q_{\\alpha}),Q_{1-\\alpha}\\big),\\quad \\bar{x}_{\\text{win}}=\\frac{1}{n}\\sum_{i=1}^{n} x_i^{\\text{win}} \\]"
  ),
  
  "ci_mean" = list(
    desc = list(
      fr = "L’intervalle de confiance (IC) à 95% de la moyenne fournit une plage plausible pour la moyenne vraie de la population, compte tenu de la variabilité et de n. IC étroit = estimation précise ; IC large = incertitude. Il utilise souvent la loi de Student lorsque l’écart-type de la population est inconnu. Interprétation correcte : si l’on répétait l’échantillonnage, ~95% des IC ainsi construits contiendraient la vraie moyenne. Prudence : petits n + forte non-normalité → envisager un bootstrap.",
      ar = "فترة الثقة 95% للمتوسط تعطي نطاقًا معقولًا لمتوسط المجتمع الحقيقي اعتمادًا على التباين وحجم العينة n. نطاق ضيق يعني دقة أعلى والواسع يعني عدم يقين أكبر. غالبًا تُحسب بتوزيع ستودنت عندما لا نعرف انحراف المجتمع. التفسير الصحيح: لو كررنا أخذ عينات وبنينا فترات بنفس الطريقة فحوالي 95% منها ستحتوي المتوسط الحقيقي. تنبيه: مع n صغير وتوزيع غير طبيعي جدًا يمكن التفكير في bootstrap."
    ),
    math = "\\[ IC_{95\\%}=\\left[\\bar{x}-t_{0.975,\\,n-1}\\frac{s}{\\sqrt{n}}\\ ;\\ \\bar{x}+t_{0.975,\\,n-1}\\frac{s}{\\sqrt{n}}\\right] \\]"
  ),
  
  "geom_mean" = list(
    desc = list(
      fr = "La moyenne géométrique convient aux phénomènes multiplicatifs (croissance, rendements). Elle correspond à un “facteur moyen” et revient à moyenner les logarithmes puis revenir à l’échelle d’origine. Elle n’est définie que pour des valeurs strictement positives (pas de 0 ni de négatifs). En présence de zéros, il faut adapter la stratégie (transformation, epsilon, autre indicateur) selon le contexte.",
      ar = "المتوسط الهندسي مناسب للظواهر المضاعِفة مثل النمو والعوائد. يمثل “عاملًا متوسطًا” ويعادل متوسط اللوغاريتمات ثم العودة للمقياس الأصلي. لا يُعرّف إلا للقيم الموجبة تمامًا (لا أصفار ولا قيم سالبة). عند وجود أصفار يجب تعديل الأسلوب (تحويل، إضافة قيمة صغيرة، أو مقياس آخر) حسب السياق."
    ),
    math = "\\[ \\text{GM}=\\left(\\prod_{i=1}^{n} x_i\\right)^{\\frac{1}{n}}=\\exp\\left(\\frac{1}{n}\\sum_{i=1}^{n}\\ln x_i\\right),\\quad x_i>0 \\]"
  ),
  
  "harm_mean" = list(
    desc = list(
      fr = "La moyenne harmonique est pertinente pour moyenner des taux, vitesses et ratios quand le dénominateur est “constant” (ex. distance fixe). Elle donne plus de poids aux petites valeurs, ce qui est souvent souhaitable pour des taux. Elle exige des valeurs positives et devient instable si certaines valeurs sont proches de zéro (car les inverses explosent).",
      ar = "المتوسط التوافقي مناسب لمتوسط المعدلات والسرعات والنِّسَب عندما يكون المقام ثابتًا (مثل مسافة ثابتة). يعطي وزنًا أكبر للقيم الصغيرة وهو منطقي غالبًا في المعدلات. يتطلب قيماً موجبة وقد يصبح غير مستقر إذا اقتربت قيم من الصفر لأن المقلوب يكبر جدًا."
    ),
    math = "\\[ \\text{HM}=\\frac{n}{\\sum_{i=1}^{n}\\frac{1}{x_i}},\\quad x_i>0 \\]"
  ),
  
  "median" = list(
    desc = list(
      fr = "La médiane est la valeur centrale : 50% des observations sont en dessous (ou égales) et 50% au-dessus (ou égales). Elle est robuste aux outliers et recommandée pour les distributions asymétriques. Elle se complète naturellement avec l’IQR pour décrire la dispersion des 50% centraux, et avec P10/P90 pour situer les queues.",
      ar = "الوسيط هو القيمة المركزية: 50% من الملاحظات أقل منها أو تساويها و50% أكبر منها أو تساويها. وهو مقاوم للقيم الشاذة ويُفضّل في التوزيعات المنحرفة. يُكمّل عادةً بـ IQR لوصف تشتت 50% الوسطى وبـ P10/P90 لفهم الأطراف."
    ),
    # math = "\\[ \\tilde{x}=\\begin{cases}x_{\\left(\\frac{n+1}{2}\\right)} & n\\ \\text{impair}\\\\ \\frac{x_{\\left(\\frac{n}{2}\\right)}+x_{\\left(\\frac{n}{2}+1\\right)}}{2} & n\\ \\text{pair}\\end{cases} \\]",
    math = "\\[
                                      \\tilde{x}=
                                      \\begin{cases}
                                      x_{\\left(\\frac{n+1}{2}\\right)} & n\\ \\text{impair}\\\\
                                      \\left(x_{\\left(\\frac{n}{2}\\right)} + x_{\\left(\\frac{n}{2}+1\\right)}\\right)/2 & n\\ \\text{pair}
                                      \\end{cases}
                                  \\]"
  
    ),
  
  "mode" = list(
    desc = list(
      fr = "Le mode est la valeur (ou catégorie) la plus fréquente. Très utile pour des variables discrètes/catégorielles. Pour des variables continues avec beaucoup de valeurs uniques, le mode “exact” peut être peu informatif : un mode par classes (histogramme/densité) est alors plus pertinent. Une distribution peut être multimodale (plusieurs modes), révélant des sous-groupes.",
      ar = "المنوال هو أكثر قيمة (أو فئة) تكرارًا. مفيد جدًا للمتغيرات المنفصلة أو الفئوية. في المتغيرات المستمرة ذات القيم الفريدة الكثيرة قد لا يكون المنوال الدقيق ذا معنى؛ منوال حسب الفئات عبر المدرج/الكثافة يكون أنسب. وقد يكون التوزيع متعدد المنوال مما يشير إلى مجموعات فرعية."
    ),
    math = "\\[ \\text{Mode}=\\arg\\max_{v}\\ \\#\\{i: x_i=v\\} \\]"
  ),
  
  "sd" = list(
    desc = list(
      fr = "L’écart-type (sd) mesure la dispersion autour de la moyenne, dans la même unité que la variable. Il est sensible aux extrêmes : quelques outliers peuvent l’augmenter fortement. Pour des distributions très asymétriques, on peut préférer MAD/IQR pour une dispersion plus robuste. À lire avec un graphique (histogramme/boxplot).",
      ar = "الانحراف المعياري (sd) يقيس التشتت حول المتوسط وبنفس وحدة المتغير. وهو حساس للقيم المتطرفة وقد يرتفع بسببها. في التوزيعات المنحرفة جدًا قد تكون MAD أو IQR أكثر مقاومة لوصف التشتت. يُفضّل تفسيره مع رسم بياني."
    ),
    math = "\\[ s=\\sqrt{\\frac{1}{n-1}\\sum_{i=1}^{n}(x_i-\\bar{x})^2} \\]"
  ),
  
  "var" = list(
    desc = list(
      fr = "La variance (var) est l’écart-type au carré. Elle est centrale dans de nombreuses formules (inférence, modèles), mais moins intuitive car exprimée en unités carrées. En description, on interprète souvent sd plutôt que var, mais var est utile pour les décompositions et comparaisons analytiques.",
      ar = "التباين (var) هو مربع الانحراف المعياري. مهم جدًا في كثير من الصيغ والنماذج لكنه أقل سهولة في التفسير لأنه بوحدات مربعة. غالبًا يُفسَّر sd بدلًا من var، لكن var مفيد في التفكيكات والمقارنات التحليلية."
    ),
    math = "\\[ s^2=\\frac{1}{n-1}\\sum_{i=1}^{n}(x_i-\\bar{x})^2 \\]"
  ),
  
  "se" = list(
    desc = list(
      fr = "L’erreur standard (SE) mesure l’incertitude sur la moyenne estimée : sd/√n. Elle diminue quand n augmente. Point clé : SE décrit la précision de l’estimation de la moyenne, pas la dispersion des données (c’est sd). On l’utilise pour construire des IC et faire des tests sur la moyenne.",
      ar = "الخطأ المعياري (SE) يقيس عدم اليقين حول متوسط العينة: sd/√n. ينخفض مع زيادة n. نقطة أساسية: SE يصف دقة تقدير المتوسط وليس تشتت البيانات (التشتت هو sd). يُستخدم لبناء فترات الثقة واختبار الفرضيات حول المتوسط."
    ),
    math = "\\[ SE(\\bar{x})=\\frac{s}{\\sqrt{n}} \\]"
  ),
  
  "cv" = list(
    desc = list(
      fr = "Le coefficient de variation (CV%) est sd rapporté à la moyenne (×100). Il compare une variabilité relative entre variables d’échelles différentes. Limite majeure : si la moyenne est proche de 0, le CV devient instable et perd son sens. Il est surtout adapté à des variables positives sur une échelle de ratio.",
      ar = "معامل التباين (CV%) هو sd مقسومًا على المتوسط (×100) ويُستخدم لمقارنة التشتت النسبي بين متغيرات بمقاييس مختلفة. حد مهم: إذا كان المتوسط قريبًا من الصفر يصبح CV غير مستقر ويفقد معناه. يناسب عادةً المتغيرات الموجبة وعلى مقياس نسبي."
    ),
    math = "\\[ CV\\% = \\frac{s}{\\bar{x}}\\times 100 \\]"
  ),
  
  "mad" = list(
    desc = list(
      fr = "Le MAD (Median Absolute Deviation) mesure la dispersion autour de la médiane : médiane(|x_i − médiane|). Il est très robuste aux outliers et souvent préférable à sd si les données sont asymétriques ou contiennent des extrêmes. Option : on peut le “mettre à l’échelle” (×1.4826) pour être comparable à sd sous normalité, selon les conventions.",
      ar = "‏MAD (الانحراف المطلق الوسيط) يقيس التشتت حول الوسيط: وسيط(|x_i − الوسيط|). وهو مقاوم جدًا للقيم الشاذة وغالبًا أفضل من sd إذا كان التوزيع منحرفًا أو يحتوي على قيم متطرفة. خيار: يمكن ضربه بـ (×1.4826) ليقارب sd تحت افتراض الطبيعية حسب العُرف."
    ),
    math = "\\[ MAD=\\operatorname{m\\acute{e}diane}\\big(|x_i-\\tilde{x}|\\big) \\quad (\\text{option: } MAD_{\\text{norm}}=1.4826\\,MAD) \\]"
  ),
  
  "iqr" = list(
    desc = list(
      fr = "L’IQR = Q3 − Q1 décrit la dispersion des 50% centraux et est robuste aux extrêmes. Il est utilisé dans les boxplots et la règle 1.5×IQR pour détecter les outliers. Un IQR petit indique une moitié centrale concentrée ; un IQR grand indique une variabilité typique élevée.",
      ar = "‏IQR = Q3 − Q1 يصف تشتت 50% الوسطى وهو مقاوم للقيم المتطرفة. يُستخدم في مخطط الصندوق وقاعدة ‎1.5×IQR‎ لاكتشاف القيم الشاذة. IQR صغير يعني تركّز النصف الأوسط، وكبير يعني تباينًا اعتياديًا أكبر."
    ),
    math = "\\[ IQR = Q_3 - Q_1 \\]"
  ),
  
  "p10" = list(
    desc = list(
      fr = "Le décile 10% (P10) est la valeur en dessous de laquelle se trouvent 10% des observations. Il décrit le bas de la distribution de manière plus stable que le minimum (moins dépendant d’un seul point extrême). Utile pour résumer la queue basse et comparer des groupes sur les faibles niveaux.",
      ar = "العُشير 10% (P10) هو القيمة التي يقع تحتها 10% من الملاحظات. يصف الطرف الأدنى بشكل أكثر ثباتًا من الحد الأدنى لأنه أقل اعتمادًا على قيمة متطرفة واحدة. مفيد لتلخيص الذيل السفلي ولمقارنة المجموعات عند المستويات المنخفضة."
    ),
    math = "\\[ P10 = Q_{0.10} \\]"
  ),
  
  "p90" = list(
    desc = list(
      fr = "Le décile 90% (P90) est la valeur en dessous de laquelle se trouvent 90% des observations (donc 10% au-dessus). Il décrit le haut de la distribution de manière robuste et complète utilement max. Il aide à comprendre les niveaux élevés sans dépendre d’un extrême unique.",
      ar = "العُشير 90% (P90) هو القيمة التي يقع تحتها 90% من الملاحظات (أي 10% أعلى منها). يصف الطرف الأعلى بشكل مقاوم ويُكمّل max. يساعد على فهم المستويات العالية دون الاعتماد على قيمة متطرفة واحدة."
    ),
    math = "\\[ P90 = Q_{0.90} \\]"
  ),
  
  "min" = list(
    desc = list(
      fr = "La valeur minimale (min) est la plus petite observation valide (hors NA). Elle peut être affectée par une valeur aberrante ; on la lit donc avec P10/Q1 pour une image plus stable. Dans certains contextes, min peut refléter un plancher réel (zéro technique/limite).",
      ar = "القيمة الصغرى (min) هي أصغر ملاحظة صالحة (باستثناء NA). قد تتأثر بقيمة شاذة لذا تُقرأ مع P10 وQ1 للحصول على صورة أكثر ثباتًا. وفي بعض السياقات قد تعكس حدًا أدنى حقيقيًا."
    ),
    math = "\\[ x_{\\min} = \\min_{1\\le i\\le n} x_i \\]"
  ),
  
  "q1" = list(
    desc = list(
      fr = "Q1 (25%) : 25% des valeurs sont ≤ Q1. Il résume le bas de la distribution de façon robuste et sert à calculer l’IQR (Q3 − Q1). Utile pour comparer les “faibles niveaux typiques” entre groupes.",
      ar = "‏Q1 (25%) : ‏25% من القيم ≤ Q1. يصف الجزء الأدنى بشكل مقاوم ويستخدم لحساب IQR (Q3 − Q1). مفيد لمقارنة المستويات المنخفضة الاعتيادية بين المجموعات."
    ),
    math = "\\[ Q_1 = Q_{0.25} \\]"
  ),
  
  "q3" = list(
    desc = list(
      fr = "Q3 (75%) : 75% des valeurs sont ≤ Q3. Avec Q1, il définit l’IQR et participe à la détection d’outliers (seuil supérieur Q3 + 1.5×IQR). Q3 décrit le haut “typique” de la distribution, plus robuste que max.",
      ar = "‏Q3 (75%) : ‏75% من القيم ≤ Q3. مع Q1 يحدد IQR ويساعد في اكتشاف القيم الشاذة (Q3 + 1.5×IQR). يصف Q3 الجزء الأعلى “الاعتيادي” وهو أكثر مقاومة من max."
    ),
    math = "\\[ Q_3 = Q_{0.75} \\]"
  ),
  
  "max" = list(
    desc = list(
      fr = "La valeur maximale (max) est la plus grande observation valide (hors NA). Elle peut être dominée par un outlier ; comparez-la à P90/Q3 (ou au seuil Q3 + 1.5×IQR) pour juger si elle est atypique. Un max extrême peut être réel (pic) ou une erreur : à vérifier.",
      ar = "القيمة العظمى (max) هي أكبر ملاحظة صالحة (باستثناء NA). قد تهيمن عليها قيمة شاذة؛ قارنها بـ P90 وQ3 (أو حد Q3 + 1.5×IQR) لتقييم شذوذها. قد تكون ذروة حقيقية أو خطأ لذا يجب التحقق."
    ),
    math = "\\[ x_{\\max} = \\max_{1\\le i\\le n} x_i \\]"
  ),
  
  "range" = list(
    desc = list(
      fr = "L’étendue (range = max − min) résume la dispersion totale mais dépend uniquement des deux extrêmes, donc elle est très sensible aux outliers. À compléter par sd/IQR (ou MAD) pour une vision plus robuste. Utile quand les extrêmes ont un sens métier, à condition de vérifier leur plausibilité.",
      ar = "المدى (range = max − min) يلخص التشتت الكلي لكنه يعتمد فقط على الطرفين لذا فهو حساس جدًا للقيم الشاذة. يُفضّل دعمه بـ sd أو IQR (أو MAD) لوصف أكثر مقاومة. يفيد عندما تكون الأطراف ذات معنى عملي مع التحقق من معقوليتها."
    ),
    math = "\\[ R = x_{\\max} - x_{\\min} \\]"
  ),
  
  "skew" = list(
    desc = list(
      fr = "La skewness mesure l’asymétrie : positive si la queue est plus longue à droite (quelques grandes valeurs), négative si elle est plus longue à gauche. Une skewness proche de 0 suggère une distribution plutôt symétrique (sans le garantir). Elle aide à décider de transformations (log/racine) et du choix de mesures robustes. Elle est sensible aux outliers : à lire avec un histogramme/QQ-plot.",
      ar = "الالتواء (Skewness) يقيس عدم التماثل: موجب عند ذيل يميني أطول وسالب عند ذيل يساري أطول. قربه من الصفر يعني تماثلًا نسبيًا (ليس ضمانًا). يساعد على اختيار التحويلات (لوغاريتم/جذر) أو المقاييس المقاومة. وهو حساس للقيم الشاذة لذا يُفسَّر مع رسم (مدرج/QQ)."
    ),
    math = "\\[ \\gamma_1 = \\frac{\\frac{1}{n}\\sum_{i=1}^{n}(x_i-\\bar{x})^3}{\\left(\\frac{1}{n}\\sum_{i=1}^{n}(x_i-\\bar{x})^2\\right)^{3/2}} \\]"
  ),
  
  "kurt" = list(
    desc = list(
      fr = "La kurtosis décrit la forme des queues et du pic. Des valeurs élevées suggèrent des queues plus lourdes (plus d’extrêmes). L’interprétation dépend de la convention : kurtosis (normale ≈ 3) ou excès (normale ≈ 0). Ici, elle sert surtout d’indicateur d’extrêmes potentiels et de non-normalité. Sensible aux outliers : à compléter par quantiles et QQ-plot.",
      ar = "التفرطح (Kurtosis) يصف شكل الذيول وحدّة القمة. القيم الأعلى قد تعني ذيولًا أثقل (قيم متطرفة أكثر). يعتمد التفسير على التعريف: kurtosis (الطبيعي ≈ 3) أو excess (الطبيعي ≈ 0). هنا نستخدمه كمؤشر لاحتمال وجود قيم متطرفة وعدم طبيعية. وهو حساس للقيم الشاذة لذا يُفضّل دعمه بالكوانتيلات وQQ-plot."
    ),
    math = "\\[ \\gamma_2 = \\frac{\\frac{1}{n}\\sum_{i=1}^{n}(x_i-\\bar{x})^4}{\\left(\\frac{1}{n}\\sum_{i=1}^{n}(x_i-\\bar{x})^2\\right)^2},\\quad \\text{Exc\\`es}=\\gamma_2-3 \\]"
  ),
  
  "shapiro" = list(
    desc = list(
      fr = "Le test de Shapiro–Wilk évalue la normalité. Si p < 0.05, on rejette souvent la normalité. Attention : avec un grand n, de petites déviations deviennent significatives ; avec un petit n, le test peut manquer de puissance. Combinez toujours avec un graphique (densité/histogramme, QQ-plot) et avec skewness/kurtosis. (Souvent limité à n ≤ 5000 selon les implémentations.)",
      ar = "اختبار شابيرو–ويلك يختبر طبيعية التوزيع. إذا p < 0.05 نرفض غالبًا الطبيعية. تنبيه: مع n كبير قد تصبح انحرافات صغيرة ذات دلالة، ومع n صغير قد يكون الاختبار ضعيف القوة. يُفضّل دائمًا دعمه برسم (مدرج/كثافة وQQ-plot) ومع الالتواء والتفرطح. (غالبًا محدود بـ n ≤ 5000 حسب التطبيق.)"
    ),
    math = "\\[ W = \\frac{\\left(\\sum_{i=1}^{n} a_i\\,x_{(i)}\\right)^2}{\\sum_{i=1}^{n}(x_i-\\bar{x})^2} \\]"
  ),
  
  "jb" = list(
    desc = list(
      fr = "Le test de Jarque–Bera (p-value) évalue la normalité via la skewness et la kurtosis : une normale a asymétrie ≈ 0 et kurtosis ≈ 3 (selon la convention). Si p < 0.05, la normalité est souvent rejetée. Comme les autres tests, il est sensible à n : avec de grands échantillons, il détecte de petites déviations. Dans cette app, JB peut être calculé via une approximation chi-deux (ddl=2) sans package externe.",
      ar = "اختبار جاركي–بيرا (قيمة p) يختبر الطبيعية اعتمادًا على الالتواء والتفرطح: التوزيع الطبيعي له التواء ≈ 0 وتفرطح ≈ 3 (حسب التعريف). إذا p < 0.05 غالبًا نرفض الطبيعية. مثل بقية الاختبارات هو حساس لحجم العينة: مع n كبير يلتقط انحرافات صغيرة. في هذا التطبيق يمكن حساب JB بتقريب كاي-تربيع بدرجتي حرية دون حزم إضافية."
    ),
    math = "\\[ JB = \\frac{n}{6}\\left(S^2 + \\frac{(K-3)^2}{4}\\right),\\quad JB\\ \\overset{H_0}{\\approx}\\ \\chi^2(2) \\]"
  ),
  
  "outliers" = list(
    desc = list(
      fr = "Détection d’outliers via la règle 1.5×IQR : une valeur est atypique si x < Q1 − 1.5×IQR ou x > Q3 + 1.5×IQR. Cette règle est robuste et correspond au boxplot. Un outlier n’est pas forcément une erreur : il peut refléter un phénomène réel mais doit être vérifié car il peut influencer moyenne/sd et certains modèles. Bon protocole : vérifier la plausibilité, puis analyser la sensibilité (avec/sans outliers) et envisager des méthodes robustes si besoin.",
      ar = "اكتشاف القيم الشاذة بقاعدة ‎1.5×IQR‎: تعتبر القيمة شاذة إذا x < Q1 − 1.5×IQR أو x > Q3 + 1.5×IQR. هذه القاعدة مقاومة وتوافق مخطط الصندوق. القيمة الشاذة ليست دائمًا خطأً لكنها تستحق التحقق لأنها قد تؤثر على المتوسط/الانحراف وبعض النماذج. بروتوكول جيد: تحقق من المعقولية ثم حلّل الحساسية (مع/بدون القيم الشاذة) واستخدم طرقًا مقاومة عند الحاجة."
    ),
    math = "\\[ \\text{Outlier si } x < Q_1 - 1.5\\,IQR\\ \\text{ ou }\\ x > Q_3 + 1.5\\,IQR,\\quad IQR=Q_3-Q_1 \\]"
  )
)

# --- NEW: dictionnaire additions (do NOT remove existing) ---
stat_dict$ad <- list(
  desc = list(
    fr = "Le test d’Anderson–Darling (AD) est un test de normalité (p-value) qui donne généralement plus de poids aux queues (extrémités) que d’autres tests. Il est donc souvent plus sensible aux écarts dans les valeurs extrêmes. Si p < 0.05, on rejette souvent la normalité. Comme tout test, avec grand n il détecte de petites déviations : à interpréter avec un QQ-plot et l’objectif (diagnostic vs hypothèse stricte).",
    ar = "اختبار أندرسون–دارلينغ (AD) هو اختبار للطبيعية (قيمة p) ويعطي وزنًا أكبر للذيول مقارنةً ببعض الاختبارات الأخرى، لذلك يكون حساسًا لانحرافات الأطراف. إذا كانت p < 0.05 فغالبًا نرفض الطبيعية. ومع n كبير قد يكتشف انحرافات صغيرة؛ يُفضّل تفسيره مع QQ-plot ومع هدف التحليل."
  ),
  math = "\\[ A^2 = -n - \\frac{1}{n}\\sum_{i=1}^{n}(2i-1)\\Big[\\ln F(x_{(i)}) + \\ln\\big(1-F(x_{(n+1-i)})\\big)\\Big] \\]"
)

stat_dict$qq <- list(
  desc = list(
    fr = "Le QQ-plot compare les quantiles observés aux quantiles théoriques d’une loi normale. Si les points suivent une droite, la normalité est plausible. Les formes d’écart sont informatives : courbure en S (queues lourdes/légères), écart systématique (asymétrie), points très éloignés en bout (outliers). Le QQ-plot est souvent plus utile que les tests seuls, surtout quand n est grand (tests trop sensibles).",
    ar = "مخطط QQ يقارن الكوانتيلات المرصودة بالكوانتيلات النظرية للتوزيع الطبيعي. إذا اتبعت النقاط خطًا مستقيمًا فهذا يدعم الطبيعية. أنماط الانحراف مفيدة: تقوس على شكل S (ذيول أثقل/أخف)، انحراف منهجي (عدم تماثل)، نقاط بعيدة جدًا عند الأطراف (قيم شاذة). غالبًا يكون QQ-plot أكثر فائدة من الاختبارات وحدها خاصةً عندما يكون n كبيرًا."
  ),
  math = "\\[ (q_i^{\\text{th}},q_i^{\\text{obs}}),\\ q_i^{\\text{obs}}=x_{(i)},\\ q_i^{\\text{th}}=\\Phi^{-1}(p_i),\\ p_i\\approx\\frac{i-0.5}{n} \\]"
)

# -------------------------------------------------------------------
# 2) JEUX DE DONNÉES DE DÉMO (beaucoup)
# -------------------------------------------------------------------
demo_choices <- c(
  "mtcars (voitures)" = "mtcars",
  "iris (fleurs)" = "iris",
  "airquality (qualité air)" = "airquality",
  "ToothGrowth (dentition)" = "ToothGrowth",
  "PlantGrowth (plantes)" = "PlantGrowth",
  "ChickWeight (poussins)" = "ChickWeight",
  "CO2 (plantes CO2)" = "CO2",
  "USArrests (crime)" = "USArrests",
  "warpbreaks (tissage)" = "warpbreaks",
  "sleep (sommesil)" = "sleep",
  "InsectSprays (insecticides)" = "InsectSprays",
  "Orange (croissance)" = "Orange",
  "npk (engrais)" = "npk",
  "attitude (attitudes)" = "attitude",
  "swiss (démographie)" = "swiss",
  "trees (arbres)" = "trees",
  "women (women)" = "women",
  "longley (macro)" = "longley",
  "stackloss (industrie)" = "stackloss",
  "cars (vitesse/arrêt)" = "cars",
  "pressure (pression)" = "pressure",
  "rock (roches)" = "rock",
  "faithful (geyser)" = "faithful",
  "faithfuld (densité geyser)" = "faithfuld",
  "diamonds (ggplot2)" = "diamonds",
  "mpg (ggplot2)" = "mpg",
  "HairEyeColor (table -> df)" = "HairEyeColor",
  "Titanic (table -> df)" = "Titanic",
  "EuStockMarkets (ts -> df)" = "EuStockMarkets",
  "Nile (ts -> df)" = "Nile",
  "AirPassengers (ts -> df)" = "AirPassengers",
  "LakeHuron (ts -> df)" = "LakeHuron",
  "UKGas (ts -> df)" = "UKGas"
)

load_demo_dataset <- function(name) {
  obj <- tryCatch(get(name, envir = asNamespace("ggplot2")), error = function(e) NULL)
  if (!is.null(obj)) return(as.data.frame(obj))
  
  tmp <- new.env(parent = emptyenv())
  ok <- tryCatch({
    data(list = name, package = "datasets", envir = tmp)
    exists(name, envir = tmp, inherits = FALSE)
  }, error = function(e) FALSE)
  
  if (ok) {
    obj <- get(name, envir = tmp, inherits = FALSE)
  } else {
    obj <- tryCatch(get(name, inherits = TRUE), error = function(e) NULL)
    if (is.null(obj)) stop("Dataset introuvable : ", name)
  }
  
  if (is.data.frame(obj)) return(as.data.frame(obj))
  if (is.matrix(obj)) return(as.data.frame(obj, stringsAsFactors = FALSE))
  
  if (is.table(obj)) {
    df <- as.data.frame(obj, stringsAsFactors = FALSE)
    if ("Freq" %in% names(df)) df <- df[df$Freq > 0, , drop = FALSE]
    return(df)
  }
  
  if (is.ts(obj)) {
    tt <- time(obj)
    if (is.matrix(obj)) {
      df <- as.data.frame(obj)
      df$time <- as.numeric(tt)
      return(df)
    }
    return(data.frame(value = as.numeric(obj), time = as.numeric(tt)))
  }
  
  as.data.frame(obj)
}

# -------------------------------------------------------------------
# 3) OUTILS STATS (robustes / safe)
# -------------------------------------------------------------------
winsorize_vec <- function(x, p = 0.10) {
  x <- x[!is.na(x)]
  if (!length(x)) return(x)
  
  lo <- as.numeric(stats::quantile(x, p, names = FALSE, type = 7))
  hi <- as.numeric(stats::quantile(x, 1 - p, names = FALSE, type = 7))
  pmin(pmax(x, lo), hi)
}

jb_pvalue <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  
  s <- moments::skewness(x)
  k <- moments::kurtosis(x) # non-excess
  jb <- (n / 6) * (s^2 + ((k - 3)^2) / 4)
  1 - stats::pchisq(jb, df = 2)
}

# NEW: Anderson-Darling p-value (optional dependency nortest)
ad_pvalue <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 5) return(NA_real_)
  if (!requireNamespace("nortest", quietly = TRUE)) return(NA_real_)
  nortest::ad.test(x)$p.value
}

iqr_bounds <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 4) return(list(lo = -Inf, hi = Inf, q1 = NA_real_, q3 = NA_real_, iqr = NA_real_))
  
  q1 <- as.numeric(stats::quantile(x, 0.25, names = FALSE, type = 7))
  q3 <- as.numeric(stats::quantile(x, 0.75, names = FALSE, type = 7))
  iqr <- q3 - q1
  
  list(
    lo = q1 - 1.5 * iqr,
    hi = q3 + 1.5 * iqr,
    q1 = q1,
    q3 = q3,
    iqr = iqr
  )
}

calculate_exhaustive_stats <- function(x) {
  n_total <- length(x)
  n_na <- sum(is.na(x))
  na_pct <- if (n_total > 0) (n_na / n_total) * 100 else NA_real_
  
  x_clean <- stats::na.omit(x)
  n <- length(x_clean)
  if (!n) return(NULL)
  
  mean_val <- mean(x_clean)
  sd_val <- stats::sd(x_clean)
  se_val <- sd_val / sqrt(n)
  
  ci_margin <- if (n > 1 && is.finite(sd_val)) stats::qt(0.975, df = n - 1) * se_val else NA_real_
  ci_str <- if (is.na(ci_margin)) {
    "N/A"
  } else {
    paste0("[", round(mean_val - ci_margin, 3), " ; ", round(mean_val + ci_margin, 3), "]")
  }
  
  q1_val <- as.numeric(stats::quantile(x_clean, 0.25, names = FALSE, type = 7))
  q3_val <- as.numeric(stats::quantile(x_clean, 0.75, names = FALSE, type = 7))
  iqr_val <- q3_val - q1_val
  
  p10_val <- as.numeric(stats::quantile(x_clean, 0.10, names = FALSE, type = 7))
  p90_val <- as.numeric(stats::quantile(x_clean, 0.90, names = FALSE, type = 7))
  
  shapiro_p <- if (n >= 3 && n <= 5000) stats::shapiro.test(x_clean)$p.value else NA_real_
  ad_p <- ad_pvalue(x_clean)
  
  outliers_n <- sum(x_clean < (q1_val - 1.5 * iqr_val) | x_clean > (q3_val + 1.5 * iqr_val))
  
  mode_val <- tryCatch({
    as.numeric(names(sort(-table(x_clean)))[1])
  }, error = function(e) NA_real_)
  
  mean_trim <- mean(x_clean, trim = 0.10)
  wins_mean <- mean(winsorize_vec(x_clean, p = 0.10))
  
  list(
    n = n,
    n_na = n_na,
    na_pct = na_pct,
    sum = sum(x_clean),
    mean = mean_val,
    mean_trim = mean_trim,
    winsor_mean = wins_mean,
    ci_mean = ci_str,
    geom_mean = if (any(x_clean <= 0)) NA_real_ else exp(mean(log(x_clean))),
    harm_mean = if (any(x_clean <= 0)) NA_real_ else 1 / mean(1 / x_clean),
    median = stats::median(x_clean),
    mode = mode_val,
    sd = sd_val,
    var = stats::var(x_clean),
    se = se_val,
    cv = ifelse(mean_val != 0, (sd_val / mean_val) * 100, NA_real_),
    mad = stats::mad(x_clean),
    p10 = p10_val,
    q1 = q1_val,
    iqr = iqr_val,
    q3 = q3_val,
    p90 = p90_val,
    min = min(x_clean),
    max = max(x_clean),
    range = max(x_clean) - min(x_clean),
    skew = moments::skewness(x_clean),
    kurt = moments::kurtosis(x_clean),
    shapiro = shapiro_p,
    ad = ad_p,                 # NEW
    jb = jb_pvalue(x_clean),
    outliers = outliers_n
  )
}

format_stat_value <- function(stat_key, val) {
  if (is.null(val) || length(val) == 0 || is.na(val[1])) return("N/A")
  
  if (is.numeric(val)) {
    if (stat_key %in% c("shapiro", "jb", "ad") && is.finite(val) && val < 0.001) return("< 0.001")
    if (stat_key == "na_pct") return(paste0(round(val, 2), "%"))
    return(round(val, 4))
  }
  
  as.character(val)
}

# -------------------------------------------------------------------
# 4) UI
# -------------------------------------------------------------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "StatsWizard Ultra Pro"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gestion des Données", tabName = "data_mgmt", icon = icon("database")),
      menuItem("Statistiques Exhaustives", tabName = "stats", icon = icon("calculator")),
      menuItem("Corrélations", tabName = "correlations", icon = icon("project-diagram"))
    ),
    hr(),
    div(
      style = "padding: 10px;",
      h4("1. Charger les Données"),
      
      radioButtons(
        "data_source", "Source :",
        choices = c("Téléverser un Fichier" = "file", "Jeux de Démo" = "demo"),
        selected = "demo"
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'file'",
        fileInput("file_upload", "Choisir un fichier", accept = c(".csv", ".xlsx", ".xls")),
        uiOutput("sheet_selector")
      ),
      
      conditionalPanel(
        condition = "input.data_source == 'demo'",
        selectInput(
          "demo_dataset", "Sélectionner un dataset :",
          choices = demo_choices,
          selected = unname(demo_choices[1])
        )
      ),
      
      hr(),
      uiOutput("sidebar_controls"),
      
      radioButtons(
        "plot_type", "Visualisation :",
        choices = c(
          "Distribution (Densité)" = "dist",
          "Dispersion (Boxplot)" = "box",
          "Normalité (QQ-plot)" = "qq"     # NEW, without removing old options
        ),
        inline = TRUE
      ),
      
      # checkboxInput("plot_log", "Échelle log10 (si valeurs > 0)", value = FALSE),
      checkboxInput("plot_drop_outliers", "Exclure les outliers du tracé", value = FALSE),
      
      br(),
      div(
        align = "center",
        actionButton(
          "show_settings", " Paramètres",
          icon = icon("cog"),
          class = "btn btn-default btn-sm"
        )
      )
    )
  ),
  
  dashboardBody(
    withMathJax(),
    tags$head(
      tags$script(HTML("
        $(document).on('shiny:value', function(event) {
          setTimeout(function() {
            if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
          }, 300);
        });
        $(document).on('shown.bs.collapse', function(e) {
          if (window.MathJax) MathJax.Hub.Queue(['Typeset', MathJax.Hub, e.target]);
        });
      ")),
      tags$style(HTML("
        .stat-card { border-top: 3px solid #3c8dbc; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .group-header { padding: 10px; background: #ecf0f5; border-left: 5px solid #222d32; margin: 20px 0; font-weight: bold;}
        .stat-row { cursor: pointer; transition: 0.2s; } .stat-row:hover { background-color: #eef7fa !important; }
        .stat-label { font-weight: 600; color: #444; }
        .stat-val { float: right; color: #005a87; font-family: monospace; font-size: 1.1em; font-weight: bold;}
        .stat-explanation { background-color: #fcfcfc; border-left: 4px solid #f39c12; padding: 15px; font-size: 0.95em; border-bottom: 1px solid #ddd; }
        .math-text { overflow-x: auto; margin-top: 10px; padding-top: 10px; border-top: 1px dashed #ccc;}
        .rtl { direction: rtl; unicode-bidi: plaintext; }
      "))
    ),
    
    tabItems(
      tabItem(
        tabName = "data_mgmt",
        valueBoxOutput("data_status", width = 12),
        box(
          title = "Aperçu du jeu de données",
          width = 12, status = "primary",
          DTOutput("raw_table")
        )
      ),
      
      tabItem(
        tabName = "stats",
        uiOutput("exhaustive_stats_ui")
      ),
      
      tabItem(
        tabName = "correlations",
        fluidRow(
          box(
            title = "Paramètres Corrélation", width = 12, status = "warning", solidHeader = TRUE,
            fluidRow(
              column(
                4,
                selectInput(
                  "corr_method", "Méthode :",
                  choices = c(
                    "Pearson" = "pearson",
                    "Spearman" = "spearman",
                    "Kendall" = "kendall"
                  ),
                  selected = "pearson"
                )
              ),
              column(
                4,
                selectInput(
                  "corr_use", "Gestion des NA :",
                  choices = c(
                    "pairwise.complete.obs (recommandé)" = "pairwise.complete.obs",
                    "complete.obs (lignes complètes uniquement)" = "complete.obs"
                  ),
                  selected = "pairwise.complete.obs"
                )
              ),
              column(
                4,
                uiOutput("corr_group_controls")   # NEW: global / group / all groups
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Matrice de Corrélation (statique)", width = 12, status = "info", solidHeader = TRUE,
            plotOutput("corr_plot", height = "600px")
          )
        ),
        fluidRow(
          box(
            title = "Matrice de Corrélation (interactive Plotly)", width = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("corr_plotly", height = "650px")
          )
        ),
        # fluidRow(
        #   box(
        #     title = "Table Corrélations (long format)", width = 12, status = "primary", solidHeader = TRUE,
        #     DTOutput("corr_table")
        #   )
        # )
      )
    )
  )
)

# -------------------------------------------------------------------
# 5) SERVER
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Options (labels) -> keys (kept all + add AD)
  all_stats_options <- c(
    "N Valides" = "n",
    "Manquants (NA)" = "n_na",
    "Manquants (NA%)" = "na_pct",
    "Somme" = "sum",
    "Moyenne" = "mean",
    "Moyenne tronquée (10%)" = "mean_trim",
    "Moyenne winsorisée (10%)" = "winsor_mean",
    "IC à 95% (Moyenne)" = "ci_mean",
    "Moyenne Géométrique" = "geom_mean",
    "Moyenne Harmonique" = "harm_mean",
    "Médiane" = "median",
    "Mode" = "mode",
    "Écart-type" = "sd",
    "Variance" = "var",
    "Erreur Standard (SE)" = "se",
    "Coeff. de Variation (CV%)" = "cv",
    "Écart Absolu Médian (MAD)" = "mad",
    "Décile 10% (P10)" = "p10",
    "1er Quartile (Q1)" = "q1",
    "Écart Interquartile (IQR)" = "iqr",
    "3ème Quartile (Q3)" = "q3",
    "Décile 90% (P90)" = "p90",
    "Minimum" = "min",
    "Maximum" = "max",
    "Étendue (Range)" = "range",
    "Asymétrie (Skewness)" = "skew",
    "Aplatissement (Kurtosis)" = "kurt",
    "Normalité (p-value Shapiro)" = "shapiro",
    "Normalité (p-value Anderson–Darling)" = "ad",  # NEW
    "Normalité (p-value Jarque–Bera)" = "jb",
    "Valeurs Aberrantes (N)" = "outliers"
  )
  
  observeEvent(input$show_settings, {
    showModal(modalDialog(
      title = "Sélectionner les statistiques à afficher",
      checkboxGroupInput(
        "selected_stats", "Choisissez les mesures :",
        choices = all_stats_options,
        selected = c("n", "mean", "median", "sd")
      ),
      footer = modalButton("Fermer"),
      size = "m",
      easyClose = TRUE
    ))
  })
  
  output$sheet_selector <- renderUI({
    req(input$data_source == "file", input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    if (ext %in% c("xls", "xlsx")) {
      sheets <- readxl::excel_sheets(input$file_upload$datapath)
      selectInput("sheet_select", "Feuille :", choices = sheets)
    }
  })
  
  read_uploaded_data <- function(path, filename, sheet = NULL) {
    ext <- tools::file_ext(filename)
    
    if (ext == "csv") {
      return(readr::read_csv(path, show_col_types = FALSE))
    }
    
    if (ext %in% c("xls", "xlsx")) {
      sheets <- readxl::excel_sheets(path)
      sheet_to_read <- if (!is.null(sheet) && sheet %in% sheets) sheet else sheets[1]
      return(readxl::read_excel(path, sheet = sheet_to_read))
    }
    
    validate(need(FALSE, "Fichier invalide (CSV ou Excel uniquement)."))
  }
  
  raw_data <- reactive({
    if (identical(input$data_source, "demo")) {
      req(input$demo_dataset)
      return(load_demo_dataset(input$demo_dataset))
    }
    
    req(input$file_upload)
    read_uploaded_data(
      path = input$file_upload$datapath,
      filename = input$file_upload$name,
      sheet = input$sheet_select
    )
  })
  
  numeric_cols <- reactive({
    df <- raw_data()
    req(df)
    names(df)[vapply(df, is.numeric, logical(1))]
  })
  
  groupable_cols <- reactive({
    df <- raw_data()
    req(df)
    names(df)[vapply(df, \(z) is.character(z) || is.factor(z), logical(1))]
  })
  
  output$sidebar_controls <- renderUI({
    df <- raw_data()
    req(df)
    
    num_cols <- numeric_cols()
    grp_cols <- groupable_cols()
    
    validate(need(length(num_cols) > 0, "Aucune colonne numérique détectée (impossible de calculer/plot)."))
    
    tagList(
      h4("2. Configurer"),
      selectInput(
        "target_vars", "Variables Cibles",
        choices = num_cols, multiple = TRUE,
        selected = num_cols[1]
      ),
      selectInput(
        "group_var", "Grouper par",
        choices = c("Aucun", grp_cols),
        selected = "Aucun"
      )
    )
  })
  
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  observeEvent(raw_data(), {
    num_cols <- numeric_cols()
    grp_cols <- groupable_cols()
    
    # target_vars
    current_targets <- input$target_vars
    keep <- intersect(current_targets %||% character(0), num_cols)
    if (!length(keep) && length(num_cols)) keep <- num_cols[1]
    updateSelectInput(session, "target_vars", choices = num_cols, selected = keep)
    
    # group_var
    valid_group <- c("Aucun", grp_cols)
    selected_group <- input$group_var %||% "Aucun"
    if (!selected_group %in% valid_group) selected_group <- "Aucun"
    updateSelectInput(session, "group_var", choices = valid_group, selected = selected_group)
  }, ignoreInit = TRUE)
  
  # Prépare un data.frame "plot-ready" (corrige le bug %in% sur valeurs dupliquées)
  prepare_plot_df <- function(df, var, drop_outliers) {
    out <- df |>
      transmute(.value = .data[[var]])
    
    if (!isTRUE(drop_outliers)) return(out)
    
    b <- iqr_bounds(out$.value)
    out |>
      filter(is.na(.value) | (.value >= b$lo & .value <= b$hi))
  }
  
  # -----------------------------------------------------------------
  # UI stats (cartes)
  # -----------------------------------------------------------------
  output$exhaustive_stats_ui <- renderUI({
    df <- raw_data()
    
    active_stats <- input$selected_stats %||% c("n", "mean", "median", "sd")
    req(df, input$target_vars, active_stats)
    
    build_card <- function(df_subset, var_name, group_label = NULL) {
      stats <- calculate_exhaustive_stats(df_subset[[var_name]])
      if (is.null(stats)) return(NULL)
      
      title_text <- if (is.null(group_label)) var_name else paste(var_name, "-", group_label)
      safe_group <- if (is.null(group_label)) "global" else make.names(group_label)
      plot_id <- paste0("plot_", make.names(var_name), "_", safe_group)
      
      list_items <- lapply(names(all_stats_options), function(stat_name) {
        stat_key <- all_stats_options[[stat_name]]
        if (!stat_key %in% active_stats) return(NULL)
        
        val <- stats[[stat_key]]
        display_val <- format_stat_value(stat_key, val)
        
        collapse_id <- paste0("collapse_", stat_key, "_", make.names(var_name), "_", safe_group)
        
        # safety: if key absent in dict, avoid error
        fr_desc <- stat_dict[[stat_key]]$desc$fr %||% "Description non disponible."
        ar_desc <- stat_dict[[stat_key]]$desc$ar %||% "لا يوجد وصف متاح."
        math_txt <- stat_dict[[stat_key]]$math %||% ""
        
        tagList(
          tags$li(
            class = "list-group-item stat-row",
            `data-toggle` = "collapse",
            `data-target` = paste0("#", collapse_id),
            icon("info-circle", class = "text-info"),
            " ",
            span(class = "stat-label", stat_name),
            span(class = "stat-val", display_val)
          ),
          tags$div(
            id = collapse_id, class = "collapse stat-explanation",
            tagList(
              p(tags$strong("FR : "), fr_desc),
              tags$p(
                tags$strong("AR : "),
                tags$span(
                  class = "rtl", lang = "ar", style = "display:block;",
                  ar_desc
                )
              )
            ),
            if (!is.null(math_txt) && nzchar(math_txt)) {
              div(class = "math-text", math_txt)
            }
          )
        )
      })
      
      list_items <- Filter(Negate(is.null), list_items)
      
      box(
        title = title_text, width = 6, status = "primary", solidHeader = TRUE,
        collapsible = TRUE, class = "stat-card",
        tags$ul(class = "list-group", list_items),
        plotOutput(outputId = plot_id, height = "220px")
      )
    }
    
    if (identical(input$group_var, "Aucun")) {
      tagList(
        div(class = "group-header", h3("Analyse Globale")),
        fluidRow(lapply(input$target_vars, function(v) build_card(df, v)))
      )
    } else {
      groups <- na.omit(unique(df[[input$group_var]]))
      tagList(lapply(groups, function(g) {
        tagList(
          div(class = "group-header", h3(paste("Groupe :", g))),
          fluidRow(lapply(input$target_vars, function(v) {
            build_card(df[df[[input$group_var]] == g, , drop = FALSE], v, g)
          }))
        )
      }))
    }
  })
  
  # -----------------------------------------------------------------
  # Plots dynamiques (par variable et par groupe) : dist / box / qq
  # -----------------------------------------------------------------
  observeEvent(
    list(raw_data(), input$target_vars, input$group_var, input$plot_type, input$plot_drop_outliers, input$plot_log),
    {
      df <- raw_data()
      req(df, input$target_vars, length(input$target_vars) >= 1)
      
      make_plot <- function(plot_df, var_name, title = NULL) {
        use_log <- isTRUE(input$plot_log) && all(plot_df$.value > 0, na.rm = TRUE)
        
        if (identical(input$plot_type, "qq")) {
          # QQ-plot (log scale not applied as axis transform; if user wants log, better to transform data first.
          # Here we keep it simple: if plot_log TRUE and data >0, we log10-transform the data for QQ.
          qq_df <- plot_df
          if (isTRUE(input$plot_log) && all(qq_df$.value > 0, na.rm = TRUE)) {
            qq_df <- qq_df |> mutate(.value = log10(.value))
          }
          
          return(
            ggplot(qq_df, aes(sample = .value)) +
              stat_qq(color = "#3c8dbc", alpha = 0.7, na.rm = TRUE) +
              stat_qq_line(color = "#d9534f", linewidth = 1, na.rm = TRUE) +
              theme_minimal() +
              labs(
                title = title,
                x = "Quantiles théoriques",
                y = if (isTRUE(input$plot_log)) paste0("Quantiles observés (", var_name, " en log10)") else "Quantiles observés"
              )
          )
        }
        
        if (identical(input$plot_type, "dist")) {
          p <- ggplot(plot_df, aes(x = .value)) +
            geom_histogram(
              aes(y = after_stat(density)),
              fill = "#3c8dbc", color = "white", bins = 30, alpha = 0.7,
              na.rm = TRUE
            ) +
            geom_density(color = "#d9534f", linewidth = 1, na.rm = TRUE) +
            theme_minimal() +
            labs(x = var_name, y = "Densité", title = title)
          
          if (use_log) p <- p + scale_x_log10()
          return(p)
        }
        
        p <- ggplot(plot_df, aes(x = "", y = .value)) +
          geom_boxplot(
            fill = "#f39c12", color = "#e67e22", alpha = 0.7,
            outlier.color = "red", outlier.size = 2.5, na.rm = TRUE
          ) +
          geom_jitter(width = 0.15, alpha = 0.25, size = 1, na.rm = TRUE) +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
          ) +
          labs(y = var_name, title = title)
        
        if (use_log) p <- p + scale_y_log10()
        p
      }
      
      # Global
      if (identical(input$group_var, "Aucun")) {
        for (v in input$target_vars) {
          local({
            v_name <- v
            plot_id <- paste0("plot_", make.names(v_name), "_global")
            
            output[[plot_id]] <- renderPlot({
              req(v_name %in% names(df))
              plot_df <- prepare_plot_df(df, v_name, input$plot_drop_outliers)
              make_plot(plot_df, v_name, title = NULL)
            })
          })
        }
        return(invisible(NULL))
      }
      
      # Par groupes
      groups <- na.omit(unique(df[[input$group_var]]))
      for (v in input$target_vars) {
        for (g in groups) {
          local({
            v_name <- v
            g_name <- g
            plot_id <- paste0("plot_", make.names(v_name), "_", make.names(g_name))
            
            output[[plot_id]] <- renderPlot({
              req(v_name %in% names(df))
              sub_df <- df[df[[input$group_var]] == g_name, , drop = FALSE]
              plot_df <- prepare_plot_df(sub_df, v_name, input$plot_drop_outliers)
              
              make_plot(plot_df, v_name, title = paste0(v_name, " (", g_name, ")"))
            })
          })
        }
      }
    },
    ignoreInit = TRUE
  )
  
  # -----------------------------------------------------------------
  # Corrélations : UI group controls (NEW)
  # -----------------------------------------------------------------
  output$corr_group_controls <- renderUI({
    df <- raw_data()
    req(df)
    
    if (is.null(input$group_var) || identical(input$group_var, "Aucun")) {
      return(tags$div(tags$strong("Scope : "), tags$span("Global")))
    }
    
    groups <- na.omit(unique(df[[input$group_var]]))
    tagList(
      selectInput(
        "corr_scope",
        "Scope :",
        choices = c(
          "Global (toutes lignes)" = "__GLOBAL__",
          "Par groupe (choisir)" = "__ONE__",
          "Tous les groupes (listés)" = "__ALL__"
        ),
        selected = "__GLOBAL__"
      ),
      conditionalPanel(
        condition = "input.corr_scope == '__ONE__'",
        selectInput("corr_group_one", "Groupe :", choices = as.character(groups))
      )
    )
  })
  
  corr_df_for_scope <- reactive({
    df <- raw_data()
    req(df)
    
    if (is.null(input$group_var) || identical(input$group_var, "Aucun")) {
      return(list(mode = "single", label = "Global", df = df))
    }
    
    scope <- input$corr_scope %||% "__GLOBAL__"
    
    if (identical(scope, "__GLOBAL__")) {
      return(list(mode = "single", label = "Global", df = df))
    }
    
    if (identical(scope, "__ONE__")) {
      req(input$corr_group_one)
      sub_df <- df[df[[input$group_var]] == input$corr_group_one, , drop = FALSE]
      return(list(mode = "single", label = paste0("Groupe: ", input$corr_group_one), df = sub_df))
    }
    
    # __ALL__
    groups <- na.omit(unique(df[[input$group_var]]))
    mats <- lapply(groups, function(g) {
      sub_df <- df[df[[input$group_var]] == g, , drop = FALSE]
      list(label = paste0("Groupe: ", g), df = sub_df)
    })
    list(mode = "multi", mats = mats)
  })
  
  corr_matrix <- reactive({
    df <- raw_data()
    req(df, input$target_vars, length(input$target_vars) > 1)
    
    num_df <- df |>
      select(all_of(input$target_vars)) |>
      select(where(is.numeric))
    
    req(ncol(num_df) > 1)
    
    round(stats::cor(num_df, use = input$corr_use, method = input$corr_method), 2)
  })
  
  # Existing static ggplot correlation plot (kept)
  output$corr_plot <- renderPlot({
    # For the original plot, keep global behavior (unchanged)
    cormat <- corr_matrix()
    cormat_melted <- as.data.frame(as.table(cormat))
    names(cormat_melted) <- c("Var1", "Var2", "Correlation")
    
    ggplot(cormat_melted, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "#d9534f", high = "#3c8dbc", mid = "white",
        midpoint = 0, limit = c(-1, 1), space = "Lab",
        name = "Corrélation"
      ) +
      geom_text(aes(label = Correlation), color = "black", size = 5) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title = element_blank(),
        panel.grid.major = element_blank()
      )
  })
  
  # NEW: Plotly correlation heatmap (global / group / all)
  corr_matrix_for_df <- function(df, target_vars, corr_use, corr_method) {
    num_df <- df |>
      select(all_of(target_vars)) |>
      select(where(is.numeric))
    
    validate(need(ncol(num_df) > 1, "Sélectionnez au moins 2 variables numériques."))
    
    round(stats::cor(num_df, use = corr_use, method = corr_method), 2)
  }
  
  output$corr_plotly <- renderPlotly({
    df <- raw_data()
    req(df, input$target_vars, length(input$target_vars) > 1)
    
    scope <- corr_df_for_scope()
    req(scope)
    
    make_heatmap <- function(cormat, title) {
      vars <- colnames(cormat)
      plot_ly(
        x = vars,
        y = vars,
        z = cormat,
        type = "heatmap",
        zmin = -1, zmax = 1,
        colors = colorRamp(c("#d9534f", "#ffffff", "#3c8dbc")),
        hovertemplate = paste(
          "<b>%{y}</b> vs <b>%{x}</b><br>",
          "Corr = %{z}<extra></extra>"
        )
      ) |>
        layout(
          title = list(text = title),
          xaxis = list(tickangle = -45),
          yaxis = list(autorange = "reversed"),
          margin = list(l = 90, r = 30, b = 120, t = 70)
        )
    }
    
    if (identical(scope$mode, "single")) {
      cm <- corr_matrix_for_df(scope$df, input$target_vars, input$corr_use, input$corr_method)
      return(make_heatmap(cm, paste0("Heatmap Corrélation (", scope$label, ")")))
    }
    
    # multi: subplot
    plots <- lapply(scope$mats, function(item) {
      cm <- tryCatch(
        corr_matrix_for_df(item$df, input$target_vars, input$corr_use, input$corr_method),
        error = function(e) NULL
      )
      if (is.null(cm)) return(NULL)
      make_heatmap(cm, item$label)
    })
    plots <- Filter(Negate(is.null), plots)
    validate(need(length(plots) > 0, "Aucune matrice de corrélation calculable pour les groupes (peut-être trop de NA ou <2 variables)."))
    
    subplot(plots, nrows = min(2, length(plots)), shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE)
  })
  
  output$corr_table <- renderDT({
    df <- raw_data()
    req(df, input$target_vars, length(input$target_vars) > 1)
    
    scope <- corr_df_for_scope()
    req(scope)
    
    melt_one <- function(cm, label) {
      as.data.frame(as.table(cm)) |>
        rename(Var1 = Var1, Var2 = Var2, Correlation = Freq) |>
        mutate(Scope = label) |>
        arrange(desc(abs(Correlation)))
    }
    
    if (identical(scope$mode, "single")) {
      cm <- corr_matrix_for_df(scope$df, input$target_vars, input$corr_use, input$corr_method)
      tab <- melt_one(cm, scope$label)
    } else {
      tabs <- lapply(scope$mats, function(item) {
        cm <- tryCatch(
          corr_matrix_for_df(item$df, input$target_vars, input$corr_use, input$corr_method),
          error = function(e) NULL
        )
        if (is.null(cm)) return(NULL)
        melt_one(cm, item$label)
      })
      tab <- bind_rows(Filter(Negate(is.null), tabs))
    }
    
    datatable(tab, options = list(pageLength = 15, scrollX = TRUE), filter = "top", rownames = FALSE)
  })
  
  # -----------------------------------------------------------------
  # Data status + table
  # -----------------------------------------------------------------
  output$data_status <- renderValueBox({
    df <- raw_data()
    if (is.null(df)) {
      valueBox("En attente", "Téléversez des données", icon = icon("upload"), color = "red")
    } else {
      valueBox(
        paste0(nrow(df), " lignes / ", ncol(df), " colonnes"),
        "Données actives",
        icon = icon("check-circle"),
        color = "green"
      )
    }
  })
  
  output$raw_table <- renderDT({
    df <- raw_data()
    req(df)
    datatable(df, options = list(pageLength = 8, scrollX = TRUE), filter = "top")
  })
}

shinyApp(ui, server)