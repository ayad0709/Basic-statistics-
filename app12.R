# Fichier : app.R
# Objectif : Tableau de bord statistique ULTIME (Normalité, Outliers, Corrélations)
# Version : +++ démos (beaucoup), SANS boutons de download
# Ajout : définitions étendues FR + AR pour chaque statistique (paragraphes)
# Auteur : R Code Wizard

library(shiny)
library(shinydashboard)
library(datasets)   # important pour data() / datasets lazy
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)
library(moments)
library(tidyr)

options(shiny.maxRequestSize = 30 * 1024^2)

# --- 1) DICTIONNAIRE STATISTIQUE (FR + AR) ---
stat_dict <- list(
  "n" = list(
    desc = list(
      fr = "Le nombre d’observations valides correspond au total de valeurs réellement utilisées dans les calculs (c’est-à-dire après exclusion des valeurs manquantes). Cette taille d’échantillon influence fortement la fiabilité des estimations : plus n est grand, plus la moyenne (et d’autres statistiques) sont généralement stables et représentatives. Attention : n ne dit rien sur la qualité des données ; il indique uniquement combien de valeurs non manquantes sont disponibles.",
      ar = "يمثل عدد الملاحظات الصالحة إجمالي القيم التي استُخدمت فعليًا في الحسابات بعد استبعاد القيم المفقودة. يؤثر حجم العينة بشكل كبير على موثوقية التقديرات: كلما كبر n كانت الإحصاءات مثل المتوسط أكثر استقرارًا وتمثيلاً. تنبيه: n لا يقيّم جودة البيانات، بل يوضح فقط عدد القيم المتاحة غير المفقودة."
    ),
    math = ""
  ),
  "n_na" = list(
    desc = list(
      fr = "Le nombre de valeurs manquantes (NA) indique combien d’observations sont absentes pour la variable. Une proportion élevée de NA peut biaiser l’analyse si les valeurs manquantes ne sont pas aléatoires (par exemple, manquantes surtout dans un sous-groupe). En descriptif, on ignore souvent les NA, mais il est utile de surveiller n_na pour juger la complétude des données et décider d’une stratégie : suppression, imputation, ou analyse spécifique du mécanisme de manque.",
      ar = "يوضح عدد القيم المفقودة (NA) كم ملاحظة غير متوفرة للمتغير. ارتفاع نسبة NA قد يسبب تحيزًا إذا كانت القيم المفقودة غير عشوائية (مثلًا: مفقودة أكثر في مجموعة معينة). غالبًا تُتجاهل NA في الوصف، لكن متابعة n_na مهمة لتقييم اكتمال البيانات واختيار استراتيجية مناسبة: حذف، تعويض/إكمال، أو دراسة سبب الفقد."
    ),
    math = ""
  ),
  "mean" = list(
    desc = list(
      fr = "La moyenne arithmétique est une mesure de tendance centrale : elle résume les données par un « centre » calculé comme la somme des valeurs divisée par n. Elle est très informative lorsque la distribution est relativement symétrique, mais elle est sensible aux valeurs extrêmes (outliers) qui peuvent la déplacer fortement. Pour des distributions asymétriques ou avec outliers, la médiane est souvent plus robuste, et il peut être pertinent de compléter par un intervalle de confiance ou une visualisation.",
      ar = "المتوسط الحسابي هو مقياس للنزعة المركزية يلخص البيانات بقيمة «مركزية» تساوي مجموع القيم مقسومًا على n. يكون مفيدًا عندما يكون التوزيع شبه متماثل، لكنه حساس للقيم المتطرفة التي قد تغيّره بشكل كبير. عند وجود انحراف أو قيم شاذة، يكون الوسيط أكثر مقاومة، ومن المفيد دعم المتوسط بفترة ثقة أو برسم بياني."
    ),
    math = "\\[ \\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i \\]"
  ),
  "ci_mean" = list(
    desc = list(
      fr = "L’intervalle de confiance (IC) à 95% de la moyenne fournit une plage de valeurs plausibles pour la moyenne vraie de la population, compte tenu de la variabilité observée et de la taille d’échantillon. Un IC étroit signifie une estimation plus précise, tandis qu’un IC large indique plus d’incertitude. Cet IC repose généralement sur l’hypothèse d’un échantillonnage « raisonnable » et utilise la loi de Student lorsque l’écart-type populationnel est inconnu. Si la distribution est très non normale et n est petit, l’IC peut être moins fiable.",
      ar = "فترة الثقة 95% للمتوسط تعطي نطاقًا من القيم المعقولة لمتوسط المجتمع الحقيقي اعتمادًا على التباين وحجم العينة. كلما كان النطاق أضيق كانت الدقة أعلى، وكلما اتسع زادت عدم اليقين. عادة تُحسب باستخدام توزيع ستودنت عندما يكون الانحراف المعياري للمجتمع غير معروف. إذا كان التوزيع بعيدًا جدًا عن الطبيعي وكان n صغيرًا فقد تقل موثوقية فترة الثقة."
    ),
    math = "\\[ IC_{95\\%} = \\left[ \\bar{x} - t_{0.975} \\frac{s}{\\sqrt{n}} \\, ; \\, \\bar{x} + t_{0.975} \\frac{s}{\\sqrt{n}} \\right] \\]"
  ),
  "geom_mean" = list(
    desc = list(
      fr = "La moyenne géométrique est adaptée aux phénomènes multiplicatifs (croissances, ratios, rendements) : elle revient à moyenner les logarithmes puis à revenir à l’échelle initiale. Elle est particulièrement utile lorsque les valeurs varient sur plusieurs ordres de grandeur. Elle n’est définie que pour des valeurs strictement positives ; si la variable contient des zéros ou des négatifs, on ne peut pas la calculer directement (il faut une transformation ou un autre indicateur).",
      ar = "المتوسط الهندسي مناسب للظواهر المضاعِفة مثل النمو والعوائد والنِّسَب، إذ يعادل أخذ متوسط اللوغاريتمات ثم الرجوع إلى المقياس الأصلي. يكون مفيدًا عندما تمتد القيم عبر مراتب كبيرة. لا يُعرّف إلا للقيم الموجبة تمامًا؛ وجود أصفار أو قيم سالبة يمنع حسابه مباشرةً (قد نحتاج تحويلًا أو مقياسًا آخر)."
    ),
    math = "\\[ \\left(\\prod_{i=1}^n x_i\\right)^{\\frac{1}{n}} \\]"
  ),
  "harm_mean" = list(
    desc = list(
      fr = "La moyenne harmonique est pertinente quand on moyenne des taux ou des vitesses (par exemple, vitesse moyenne sur des distances égales). Elle donne plus de poids aux petites valeurs, ce qui peut être souhaitable pour des ratios. Elle nécessite des valeurs strictement positives, et elle devient instable si certaines valeurs sont très proches de zéro (car on prend des inverses).",
      ar = "المتوسط التوافقي مناسب عند حساب متوسط المعدلات أو السرعات (مثل متوسط السرعة لمسافات متساوية). يعطي وزنًا أكبر للقيم الصغيرة، وهو مفيد في سياق النِّسَب. يتطلب قيماً موجبة تمامًا وقد يصبح غير مستقر إذا كانت بعض القيم قريبة جدًا من الصفر لأن الحساب يعتمد على مقلوبات القيم."
    ),
    math = "\\[ \\frac{n}{\\sum_{i=1}^n \\frac{1}{x_i}} \\]"
  ),
  "median" = list(
    desc = list(
      fr = "La médiane est la valeur centrale : 50% des observations sont en dessous et 50% au-dessus. Elle est dite robuste, car une valeur extrême, même très grande, influence peu sa valeur. Elle est donc recommandée pour des distributions asymétriques (revenus, durées, tailles) ou lorsque des outliers sont présents. Elle est souvent complétée par l’IQR pour décrire la dispersion centrale.",
      ar = "الوسيط هو القيمة المركزية بحيث تقع 50% من الملاحظات تحتها و50% فوقها. يُعد مقاومًا للقيم المتطرفة لأن قيمة شاذة كبيرة جدًا لا تغيّره كثيرًا. لذلك يُفضّل في التوزيعات المنحرفة (الدخول، المدد، الأحجام) أو عند وجود قيم شاذة. غالبًا يُكمل بـ IQR لوصف تشتت الجزء الأوسط."
    ),
    math = ""
  ),
  "mode" = list(
    desc = list(
      fr = "Le mode est la valeur la plus fréquente dans la série. Il est très utile pour des variables catégorielles ou discrètes (tailles, classes, notes). Pour des variables continues (mesures avec beaucoup de valeurs distinctes), le mode « exact » peut être peu informatif car la plupart des valeurs n’apparaissent qu’une fois ; dans ce cas, un mode par classes (histogramme) ou une estimation de densité est souvent plus pertinente.",
      ar = "المنوال هو أكثر قيمة تكرارًا في البيانات. يكون مفيدًا جدًا للمتغيرات الفئوية أو المنفصلة. في المتغيرات المستمرة ذات القيم المتعددة والفريدة قد لا يكون المنوال «الدقيق» ذا معنى لأن أغلب القيم تظهر مرة واحدة؛ عندها يكون المنوال حسب الفئات (عبر المدرج) أو تقدير الكثافة أكثر فائدة."
    ),
    math = ""
  ),
  "sd" = list(
    desc = list(
      fr = "L’écart-type mesure la dispersion autour de la moyenne : إذا كان صغيرًا فالقيم قريبة من المتوسط، وإذا كان كبيرًا فالقيم متباعدة. Il s’exprime dans la même unité que la variable (kg, m, etc.), ce qui facilite l’interprétation. Il est sensible aux valeurs extrêmes et suppose implicitement que la moyenne représente bien le centre ; avec une distribution très asymétrique, des mesures robustes (MAD, IQR) peuvent être plus appropriées.",
      ar = "الانحراف المعياري يقيس مقدار التشتت حول المتوسط: إذا كان صغيرًا فالقيم قريبة من المتوسط، وإذا كان كبيرًا فالقيم متباعدة. يُقاس بنفس وحدة المتغير، مما يسهل تفسيره. وهو حساس للقيم المتطرفة ويفترض ضمنيًا أن المتوسط يمثل المركز جيدًا؛ في التوزيعات المنحرفة قد تكون مقاييس أكثر مقاومة مثل MAD أو IQR أنسب."
    ),
    math = "\\[ s = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2} \\]"
  ),
  "var" = list(
    desc = list(
      fr = "La variance est l’écart-type au carré. Elle apparaît souvent dans les formules théoriques (modèles statistiques) mais elle est moins intuitive à lire car elle est en unités « carrées ». En pratique, on interprète plus souvent l’écart-type. Comme l’écart-type, la variance est sensible aux outliers.",
      ar = "التباين هو مربع الانحراف المعياري. يظهر كثيرًا في الصيغ النظرية والنماذج الإحصائية، لكنه أقل سهولة في التفسير لأنه بوحدات «مربعة». عمليًا يُفسَّر الانحراف المعياري أكثر. وكما في الانحراف المعياري، التباين حساس للقيم الشاذة."
    ),
    math = "\\[ s^2 = \\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2 \\]"
  ),
  "se" = list(
    desc = list(
      fr = "L’erreur standard (SE) de la moyenne quantifie l’incertitude sur la moyenne estimée : elle est égale à l’écart-type divisé par √n. Ainsi, à variabilité identique, doubler la taille d’échantillon réduit l’erreur standard. Le SE n’est pas une dispersion des données (ça c’est sd), mais une dispersion de l’estimation de la moyenne : il sert à construire des intervalles de confiance et des tests.",
      ar = "الخطأ المعياري (SE) للمتوسط يقيس عدم اليقين حول متوسط العينة: يساوي الانحراف المعياري مقسومًا على √n. لذلك، مع نفس التشتت، زيادة حجم العينة تقلل SE. ‏SE ليس تشتت البيانات نفسها (ذلك sd)، بل تشتت تقدير المتوسط، ويُستخدم لبناء فترات الثقة وإجراء الاختبارات."
    ),
    math = "\\[ SE = \\frac{s}{\\sqrt{n}} \\]"
  ),
  "cv" = list(
    desc = list(
      fr = "Le coefficient de variation (CV%) exprime la dispersion relative : sd rapporté à la moyenne ثم مضروبًا في 100. Il est utile pour comparer la variabilité entre variables de différentes unités ou échelles. Interprétation : un CV élevé signifie une grande variabilité par rapport au niveau moyen. Attention si la moyenne est proche de 0 : le CV peut devenir très grand ou instable et perdre son sens.",
      ar = "معامل التباين (CV%) يعبر عن التشتت النسبي: الانحراف المعياري مقسومًا على المتوسط ثم ×100. يفيد لمقارنة التباين بين متغيرات بوحدات أو مقاييس مختلفة. CV المرتفع يعني تشتتًا كبيرًا مقارنةً بالمستوى المتوسط. تنبيه: إذا كان المتوسط قريبًا من الصفر قد يصبح CV كبيرًا جدًا أو غير مستقر ويفقد معناه."
    ),
    math = "\\[ CV = \\frac{s}{\\bar{x}} \\times 100 \\]"
  ),
  "mad" = list(
    desc = list(
      fr = "Le MAD (Median Absolute Deviation) est une mesure robuste de dispersion : on calcule les écarts absolus à la médiane puis on en prend la médiane. Il est très peu influencé par les valeurs aberrantes, contrairement à sd. Le MAD est utile quand les données contiennent des outliers ou عندما تكون التوزيعات غير متماثلة. On peut l’utiliser comme alternative robuste à l’écart-type.",
      ar = "‏MAD (الانحراف المطلق الوسيط) هو مقياس مقاوم للتشتت: نحسب الانحرافات المطلقة عن الوسيط ثم نأخذ وسيطها. لا يتأثر كثيرًا بالقيم الشاذة بعكس sd. يكون مفيدًا عندما تحتوي البيانات على قيم متطرفة أو عندما يكون التوزيع غير متماثل. يمكن اعتباره بديلًا مقاومًا للانحراف المعياري."
    ),
    math = "\\[ MAD = \\text{médiane}(|x_i - \\tilde{x}|) \\]"
  ),
  "iqr" = list(
    desc = list(
      fr = "L’IQR (Interquartile Range) est la différence Q3 − Q1. Il représente la dispersion des 50% centraux des données, ce qui le rend robuste aux valeurs extrêmes. Il est souvent utilisé pour décrire la variabilité avec la médiane, et il sert aussi à détecter des outliers via la règle 1.5×IQR. Un IQR grand signifie que la moitié centrale des données est très étalée.",
      ar = "‏IQR (المدى الربيعي) هو الفرق بين الربع الثالث Q3 والربع الأول Q1. يمثل تشتت 50% الوسطى من البيانات، لذلك فهو مقاوم للقيم المتطرفة. يُستخدم كثيرًا مع الوسيط لوصف التباين، ويُستعمل كذلك لاكتشاف القيم الشاذة بقاعدة ‎1.5×IQR‎. IQR الكبير يعني أن النصف الأوسط من البيانات متباعد."
    ),
    math = "\\[ IQR = Q_3 - Q_1 \\]"
  ),
  "min" = list(
    desc = list(
      fr = "La valeur minimale est la plus petite observation (hors NA). Elle informe sur l’extrémité basse de la distribution mais peut être influencée par une seule observation aberrante. Elle est surtout utile lorsqu’on la compare à d’autres quantiles (Q1, médiane) pour comprendre la forme et la présence d’extrêmes.",
      ar = "القيمة الصغرى هي أصغر ملاحظة (باستثناء NA). تعطي فكرة عن الطرف الأدنى للتوزيع لكنها قد تتأثر بملاحظة شاذة واحدة. تكون أكثر فائدة عند مقارنتها مع الكوانتيلات مثل Q1 والوسيط لفهم الشكل ووجود القيم المتطرفة."
    ),
    math = ""
  ),
  "q1" = list(
    desc = list(
      fr = "Le premier quartile (Q1) est la valeur en dessous de laquelle se trouvent 25% des observations. C’est un indicateur robuste de la partie basse de la distribution. Il est utilisé avec Q3 et la médiane pour résumer les données sans être trop sensible aux extrêmes.",
      ar = "الربع الأول (Q1) هو القيمة التي تقع تحتها 25% من الملاحظات. يصف الجزء الأدنى من التوزيع بشكل مقاوم للقيم المتطرفة. يُستخدم مع Q3 والوسيط لتلخيص البيانات دون التأثر الشديد بالنهايات."
    ),
    math = ""
  ),
  "q3" = list(
    desc = list(
      fr = "Le troisième quartile (Q3) est la valeur en dessous de laquelle se trouvent 75% des observations (ou au-dessus de laquelle se trouvent 25%). Il décrit la partie haute de la distribution de façon robuste. Avec Q1, il définit l’IQR et sert à repérer des valeurs atypiques.",
      ar = "الربع الثالث (Q3) هو القيمة التي تقع تحتها 75% من الملاحظات (أو فوقها 25%). يصف الجزء الأعلى من التوزيع بطريقة مقاومة. مع Q1 يحدد IQR ويُستخدم لاكتشاف القيم غير المعتادة."
    ),
    math = ""
  ),
  "max" = list(
    desc = list(
      fr = "La valeur maximale est la plus grande observation (hors NA). Comme le minimum, elle peut être dominée par une valeur extrême et ne représente pas forcément le comportement général. Elle est utile pour évaluer l’étendue et détecter de possibles anomalies ou contraintes (plafonds, censure).",
      ar = "القيمة العظمى هي أكبر ملاحظة (باستثناء NA). مثل الصغرى، قد تهيمن عليها قيمة متطرفة ولا تمثل السلوك العام. تفيد في تقدير المدى واكتشاف شواذ أو حدود (سقف قياس، تقطيع)."
    ),
    math = ""
  ),
  "range" = list(
    desc = list(
      fr = "L’étendue (range) est max − min. C’est un résumé très simple de la dispersion totale, mais il est très sensible aux valeurs extrêmes car il ne dépend que des deux observations les plus extrêmes. Il sert surtout comme indication rapide ou pour vérifier des limites, mais il doit être complété par sd/IQR pour une vision plus stable.",
      ar = "المدى هو (أكبر قيمة − أصغر قيمة). هو مقياس بسيط للتشتت الكلي لكنه حساس جدًا للقيم المتطرفة لأنه يعتمد فقط على القيمتين الطرفيتين. يفيد كمؤشر سريع أو للتحقق من الحدود، ويُفضّل دعمه بـ sd أو IQR للحصول على وصف أكثر ثباتًا."
    ),
    math = "\\[ R = x_{\\max} - x_{\\min} \\]"
  ),
  "skew" = list(
    desc = list(
      fr = "L’asymétrie (skewness) mesure la dissymétrie de la distribution. Une skewness positive indique une queue plus longue à droite (quelques valeurs grandes), tandis qu’une skewness négative indique une queue plus longue à gauche. Une valeur proche de 0 suggère une distribution relativement symétrique. L’asymétrie peut guider le choix de transformations (log, racine) ou l’utilisation de statistiques robustes.",
      ar = "الالتواء (Skewness) يقيس عدم تماثل التوزيع. الالتواء الموجب يعني ذيلًا أطول يمينًا (قيم كبيرة قليلة)، والسالب يعني ذيلًا أطول يسارًا. قيمة قريبة من الصفر تشير إلى تماثل نسبي. يساعد الالتواء على اختيار تحويلات (لوغاريتم، جذر) أو استخدام مقاييس مقاومة."
    ),
    math = "\\[ \\gamma_1 = \\frac{\\sum (x_i - \\bar{x})^3}{(n-1)s^3} \\]"
  ),
  "kurt" = list(
    desc = list(
      fr = "La kurtosis (aplatissement) décrit la forme des queues et le « pic » de la distribution. Une kurtosis plus élevée est souvent associée à des queues plus lourdes (plus de valeurs extrêmes) et un pic plus marqué, tandis qu’une kurtosis plus faible suggère des queues plus légères. L’interprétation dépend de la convention (avec ou sans « excès »). Dans tous les cas, elle est utile pour suspecter la présence d’extrêmes fréquents.",
      ar = "التفرطح (Kurtosis) يصف شكل الذيول وحدّة القمة في التوزيع. القيم الأعلى ترتبط غالبًا بذيول أثقل (قيم متطرفة أكثر) وقمة أشد، بينما القيم الأقل تعني ذيولًا أخف. يعتمد التفسير على التعريف المستخدم (مع «الزيادة» أو بدونها). بشكل عام يفيد في الاشتباه بوجود قيم متطرفة متكررة."
    ),
    math = "\\[ K = \\frac{\\sum (x_i - \\bar{x})^4}{(n-1)s^4} \\]"
  ),
  "shapiro" = list(
    desc = list(
      fr = "Le test de Shapiro-Wilk évalue si les données peuvent provenir d’une distribution normale. La p-value mesure la compatibilité avec la normalité : si p < 0.05, on rejette souvent l’hypothèse de normalité. Ce test est sensible à la taille d’échantillon : avec de grands n, de petites déviations peuvent devenir significatives. Il faut donc le lire avec un histogramme/densité et un QQ-plot (si disponible), et tenir compte du contexte.",
      ar = "اختبار شابيرو-ويلك يتحقق مما إذا كانت البيانات قد تأتي من توزيع طبيعي. قيمة p تقيس مدى التوافق مع الطبيعية: إذا كانت p < 0.05 فعادة نرفض فرضية الطبيعية. الاختبار حساس لحجم العينة: مع n كبير قد تصبح انحرافات صغيرة ذات دلالة. لذا يُستحسن قراءته مع الرسم (كثافة/مدرج) ومخطط QQ إن توفر، ومع مراعاة السياق."
    ),
    math = "\\[ W = \\frac{\\left(\\sum a_i x_{(i)}\\right)^2}{\\sum(x_i - \\bar{x})^2} \\]"
  ),
  "outliers" = list(
    desc = list(
      fr = "Les valeurs aberrantes (outliers) sont détectées ici par la règle 1.5×IQR : une observation est considérée atypique si elle est inférieure à Q1 − 1.5×IQR ou supérieure à Q3 + 1.5×IQR. Cette méthode est simple et robuste, souvent utilisée avec les boxplots. Un outlier n’est pas forcément une erreur : il peut refléter un phénomène réel, mais il mérite vérification car il peut influencer fortement la moyenne, sd, et certains modèles. L’idéal est de combiner détection automatique + examen métier.",
      ar = "يتم اكتشاف القيم الشاذة هنا بقاعدة ‎1.5×IQR‎: تعتبر الملاحظة شاذة إذا كانت أقل من ‎Q1 − 1.5×IQR‎ أو أكبر من ‎Q3 + 1.5×IQR‎. هذه الطريقة بسيطة ومقاومة وغالبًا تُستخدم مع مخطط الصندوق. القيمة الشاذة ليست بالضرورة خطأً؛ قد تمثل ظاهرة حقيقية لكنها تستحق التحقق لأنها قد تؤثر بقوة على المتوسط والانحراف المعياري وبعض النماذج. الأفضل الجمع بين الكشف الآلي والفحص السياقي."
    ),
    math = "\\[ \\text{Outlier si } x < Q_1 - 1.5 \\times IQR \\text{ ou } x > Q_3 + 1.5 \\times IQR \\]"
  )
)

# --- 2) JEUX DE DONNÉES DE DÉMO (beaucoup) ---
demo_choices <- c(
  # datasets (data.frame)
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
  
  # ggplot2
  "diamonds (ggplot2)" = "diamonds",
  "mpg (ggplot2)" = "mpg",
  
  # tables -> df
  "HairEyeColor (table -> df)" = "HairEyeColor",
  "Titanic (table -> df)" = "Titanic",
  
  # ts -> df
  "EuStockMarkets (ts -> df)" = "EuStockMarkets",
  "Nile (ts -> df)" = "Nile",
  "AirPassengers (ts -> df)" = "AirPassengers",
  "LakeHuron (ts -> df)" = "LakeHuron",
  "UKGas (ts -> df)" = "UKGas"
)

# --- Chargement robuste d'un dataset demo ---
load_demo_dataset <- function(name) {
  # 1) ggplot2 d'abord
  obj <- tryCatch(get(name, envir = asNamespace("ggplot2")), error = function(e) NULL)
  if (!is.null(obj)) return(as.data.frame(obj))
  
  # 2) datasets via data() (robuste mtcars/iris/ts etc.)
  tmp <- new.env(parent = emptyenv())
  ok <- tryCatch({
    data(list = name, package = "datasets", envir = tmp)
    exists(name, envir = tmp, inherits = FALSE)
  }, error = function(e) FALSE)
  
  if (ok) {
    obj <- get(name, envir = tmp, inherits = FALSE)
  } else {
    # 3) fallback
    obj <- tryCatch(get(name, inherits = TRUE), error = function(e) NULL)
    if (is.null(obj)) stop("Dataset introuvable : ", name)
  }
  
  # 4) conversion stable
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
    } else {
      return(data.frame(value = as.numeric(obj), time = as.numeric(tt)))
    }
  }
  
  as.data.frame(obj)
}

# --- 3) UI ---
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
    div(style = "padding: 10px;",
        h4("1. Charger les Données"),
        
        radioButtons("data_source", "Source :",
                     choices = c("Téléverser un Fichier" = "file", "Jeux de Démo" = "demo"),
                     selected = "demo"),
        
        conditionalPanel(
          condition = "input.data_source == 'file'",
          fileInput("file_upload", "Choisir un fichier", accept = c(".csv", ".xlsx", ".xls")),
          uiOutput("sheet_selector")
        ),
        
        conditionalPanel(
          condition = "input.data_source == 'demo'",
          selectInput("demo_dataset", "Sélectionner un dataset :",
                      choices = demo_choices,
                      selected = unname(demo_choices[1])),
          # helpText("Astuce : certains datasets sont des séries temporelles (ts) ou des tables -> conversion automatique.")
        ),
        
        hr(),
        uiOutput("sidebar_controls"),
        
        radioButtons("plot_type", "Visualisation :",
                     choices = c("Distribution (Densité)" = "dist",
                                 "Dispersion (Boxplot)" = "box"),
                     inline = TRUE),
        
        # checkboxInput("plot_log", "Échelle log10 (si valeurs > 0)", value = FALSE),
        checkboxInput("plot_drop_outliers", "Exclure les outliers du tracé", value = FALSE),
        
        br(),
        div(align = "center",
            actionButton("show_settings", " Paramètres", icon = icon("cog"),
                         class = "btn btn-default btn-sm")
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
      tabItem(tabName = "data_mgmt",
              valueBoxOutput("data_status", width = 12),
              box(title = "Aperçu du jeu de données",
                  width = 12, status="primary",
                  DTOutput("raw_table"))),
      
      tabItem(tabName = "stats",
              uiOutput("exhaustive_stats_ui")),
      
      tabItem(tabName = "correlations",
              fluidRow(
                box(title = "Paramètres Corrélation", width = 12, status = "warning", solidHeader = TRUE,
                    fluidRow(
                      column(6,
                             selectInput("corr_method", "Méthode :",
                                         choices = c("Pearson" = "pearson",
                                                     "Spearman" = "spearman",
                                                     "Kendall" = "kendall"),
                                         selected = "pearson")
                      ),
                      column(6,
                             selectInput("corr_use", "Gestion des NA :",
                                         choices = c("pairwise.complete.obs (recommandé)" = "pairwise.complete.obs",
                                                     "complete.obs (lignes complètes uniquement)" = "complete.obs"),
                                         selected = "pairwise.complete.obs")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Matrice de Corrélation", width = 12, status = "info", solidHeader = TRUE,
                    plotOutput("corr_plot", height = "600px"))
              )
      )
    )
  )
)

# --- 4) SERVER ---
server <- function(input, output, session) {
  
  all_stats_options <- c(
    "N Valides" = "n",
    "Manquants (NA)" = "n_na",
    "Moyenne" = "mean",
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
    "Écart Interquartile (IQR)" = "iqr",
    "Minimum" = "min",
    "1er Quartile (Q1)" = "q1",
    "3ème Quartile (Q3)" = "q3",
    "Maximum" = "max",
    "Étendue (Range)" = "range",
    "Asymétrie (Skewness)" = "skew",
    "Aplatissement (Kurtosis)" = "kurt",
    "Normalité (p-value Shapiro)" = "shapiro",
    "Valeurs Aberrantes (N)" = "outliers"
  )
  
  observeEvent(input$show_settings, {
    showModal(modalDialog(
      title = "Sélectionner les statistiques à afficher",
      checkboxGroupInput(
        "selected_stats", "Choisissez les mesures :",
        choices = all_stats_options,
        selected = c("n", "mean", "ci_mean", "median", "sd", "skew", "shapiro", "outliers")
      ),
      footer = modalButton("Fermer"),
      size = "m", easyClose = TRUE
    ))
  })
  
  output$sheet_selector <- renderUI({
    req(input$data_source == "file", input$file_upload)
    ext <- tools::file_ext(input$file_upload$name)
    if (ext %in% c("xls", "xlsx")) {
      selectInput("sheet_select", "Feuille :",
                  choices = readxl::excel_sheets(input$file_upload$datapath))
    }
  })
  
  raw_data <- reactive({
    if (input$data_source == "demo") {
      req(input$demo_dataset)
      load_demo_dataset(input$demo_dataset)
    } else {
      req(input$file_upload)
      ext <- tools::file_ext(input$file_upload$name)
      path <- input$file_upload$datapath
      
      if (ext == "csv") {
        readr::read_csv(path, show_col_types = FALSE)
      } else if (ext %in% c("xls", "xlsx")) {
        sheets <- readxl::excel_sheets(path)
        sheet_to_read <- if (!is.null(input$sheet_select) && input$sheet_select %in% sheets) input$sheet_select else sheets[1]
        readxl::read_excel(path, sheet = sheet_to_read)
      } else {
        validate(need(FALSE, "Fichier invalide (CSV ou Excel uniquement)."))
      }
    }
  })
  
  numeric_cols <- reactive({
    df <- raw_data(); req(df)
    names(df)[vapply(df, is.numeric, logical(1))]
  })
  
  groupable_cols <- reactive({
    df <- raw_data(); req(df)
    names(df)[vapply(df, function(z) is.character(z) || is.factor(z), logical(1))]
  })
  
  output$sidebar_controls <- renderUI({
    df <- raw_data(); req(df)
    num_cols <- numeric_cols()
    grp_cols <- groupable_cols()
    
    validate(need(length(num_cols) > 0, "Aucune colonne numérique détectée (impossible de calculer/plot)."))
    
    tagList(
      h4("2. Configurer"),
      selectInput("target_vars", "Variables Cibles",
                  choices = num_cols, multiple = TRUE,
                  selected = num_cols[1]),
      selectInput("group_var", "Grouper par",
                  choices = c("Aucun", grp_cols),
                  selected = "Aucun")
    )
  })
  
  # garder sélections valides si dataset change
  observeEvent(raw_data(), {
    num_cols <- numeric_cols()
    grp_cols <- groupable_cols()
    
    if (!is.null(input$target_vars)) {
      keep <- intersect(input$target_vars, num_cols)
      if (length(keep) == 0 && length(num_cols) > 0) keep <- num_cols[1]
      updateSelectInput(session, "target_vars", choices = num_cols, selected = keep)
    }
    
    valid_group <- c("Aucun", grp_cols)
    if (!is.null(input$group_var) && !(input$group_var %in% valid_group)) {
      updateSelectInput(session, "group_var", choices = valid_group, selected = "Aucun")
    } else {
      updateSelectInput(session, "group_var", choices = valid_group)
    }
  }, ignoreInit = TRUE)
  
  calculate_exhaustive_stats <- function(x) {
    x_clean <- na.omit(x)
    n <- length(x_clean)
    if (n == 0) return(NULL)
    
    mean_val <- mean(x_clean)
    sd_val <- sd(x_clean)
    se_val <- sd_val / sqrt(n)
    
    ci_margin <- if (n > 1 && is.finite(sd_val)) qt(0.975, df = n - 1) * se_val else NA_real_
    
    q1_val <- as.numeric(quantile(x_clean, 0.25, names = FALSE))
    q3_val <- as.numeric(quantile(x_clean, 0.75, names = FALSE))
    iqr_val <- q3_val - q1_val
    
    shapiro_p <- if (n >= 3 && n <= 5000) shapiro.test(x_clean)$p.value else NA_real_
    outliers_n <- sum(x_clean < (q1_val - 1.5 * iqr_val) | x_clean > (q3_val + 1.5 * iqr_val))
    
    mode_val <- tryCatch({
      as.numeric(names(sort(-table(x_clean)))[1])
    }, error = function(e) NA_real_)
    
    list(
      n = n,
      n_na = sum(is.na(x)),
      mean = mean_val,
      ci_mean = if (!is.na(ci_margin)) paste0("[", round(mean_val - ci_margin, 3), " ; ", round(mean_val + ci_margin, 3), "]") else "N/A",
      geom_mean = if (any(x_clean <= 0)) NA_real_ else exp(mean(log(x_clean))),
      harm_mean = if (any(x_clean <= 0)) NA_real_ else 1 / mean(1 / x_clean),
      median = median(x_clean),
      mode = mode_val,
      sd = sd_val,
      var = var(x_clean),
      se = se_val,
      cv = ifelse(mean_val != 0, (sd_val / mean_val) * 100, NA_real_),
      mad = mad(x_clean),
      iqr = iqr_val,
      min = min(x_clean),
      q1 = q1_val,
      q3 = q3_val,
      max = max(x_clean),
      range = max(x_clean) - min(x_clean),
      skew = moments::skewness(x_clean),
      kurt = moments::kurtosis(x_clean),
      shapiro = shapiro_p,
      outliers = outliers_n
    )
  }
  
  output$exhaustive_stats_ui <- renderUI({
    df <- raw_data()
    active_stats <- if (is.null(input$selected_stats)) c("n", "mean", "ci_mean", "sd", "skew", "shapiro", "outliers") else input$selected_stats
    req(df, input$target_vars, active_stats)
    
    build_card <- function(data_subset, var_name, group_label = NULL) {
      stats <- calculate_exhaustive_stats(data_subset[[var_name]])
      if (is.null(stats)) return(NULL)
      
      title_text <- if (is.null(group_label)) var_name else paste(var_name, "-", group_label)
      safe_group <- if (is.null(group_label)) "global" else make.names(group_label)
      plot_id <- paste0("plot_", make.names(var_name), "_", safe_group)
      
      list_items <- lapply(names(all_stats_options), function(stat_name) {
        stat_key <- all_stats_options[[stat_name]]
        if (stat_key %in% active_stats) {
          val <- stats[[stat_key]]
          display_val <-
            if (is.null(val) || length(val) == 0 || is.na(val[1])) "N/A"
          else if (is.numeric(val)) {
            if (stat_key == "shapiro" && is.finite(val) && val < 0.001) "< 0.001" else round(val, 4)
          } else val
          
          collapse_id <- paste0("collapse_", stat_key, "_", make.names(var_name), "_", safe_group)
          
          tagList(
            tags$li(
              class = "list-group-item stat-row",
              `data-toggle` = "collapse",
              `data-target` = paste0("#", collapse_id),
              icon("info-circle", class = "text-info"), " ",
              span(class = "stat-label", stat_name),
              span(class = "stat-val", display_val)
            ),
            tags$div(
              id = collapse_id, class = "collapse stat-explanation",
              tagList(
                p(strong("FR : "), stat_dict[[stat_key]]$desc$fr),
                tags$p(
                  tags$strong("AR : "),
                  tags$span(class = "rtl", lang = "ar", style = "display:block;",
                            stat_dict[[stat_key]]$desc$ar)
                )
              ),
              if (stat_dict[[stat_key]]$math != "") div(class="math-text", stat_dict[[stat_key]]$math) else NULL
            )
          )
        } else NULL
      })
      list_items <- Filter(Negate(is.null), list_items)
      
      box(
        title = title_text, width = 6, status = "primary", solidHeader = TRUE,
        collapsible = TRUE, class = "stat-card",
        tags$ul(class = "list-group", list_items),
        plotOutput(outputId = plot_id, height = "220px")
      )
    }
    
    if (input$group_var == "Aucun") {
      tagList(
        div(class = "group-header", h3("Analyse Globale")),
        fluidRow(lapply(input$target_vars, function(v) build_card(df, v)))
      )
    } else {
      groups <- na.omit(unique(df[[input$group_var]]))
      tagList(lapply(groups, function(g) {
        tagList(
          div(class = "group-header", h3(paste("Groupe :", g))),
          fluidRow(lapply(input$target_vars, function(v) build_card(df[df[[input$group_var]] == g, , drop = FALSE], v, g)))
        )
      }))
    }
  })
  
  drop_outliers_iqr <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) < 4) return(x)
    q1 <- as.numeric(quantile(x, 0.25))
    q3 <- as.numeric(quantile(x, 0.75))
    iqr <- q3 - q1
    x[x >= (q1 - 1.5 * iqr) & x <= (q3 + 1.5 * iqr)]
  }
  
  # Plots dynamiques
  observe({
    df <- raw_data()
    req(df, input$target_vars, input$plot_type)
    
    for (v in input$target_vars) {
      if (input$group_var == "Aucun") {
        local({
          v_name <- v
          plot_id <- paste0("plot_", make.names(v_name), "_global")
          
          output[[plot_id]] <- renderPlot({
            req(v_name %in% names(df))
            plot_df <- df
            
            if (isTRUE(input$plot_drop_outliers)) {
              keep_vals <- drop_outliers_iqr(plot_df[[v_name]])
              plot_df <- plot_df[plot_df[[v_name]] %in% keep_vals | is.na(plot_df[[v_name]]), , drop = FALSE]
            }
            
            use_log <- isTRUE(input$plot_log) && all(plot_df[[v_name]] > 0, na.rm = TRUE)
            
            if (input$plot_type == "dist") {
              p <- ggplot(plot_df, aes(x = .data[[v_name]])) +
                geom_histogram(aes(y = after_stat(density)),
                               fill = "#3c8dbc", color = "white", bins = 30, alpha = 0.7) +
                geom_density(color = "#d9534f", linewidth = 1, na.rm = TRUE) +
                theme_minimal() +
                labs(x = v_name, y = "Densité")
              if (use_log) p <- p + scale_x_log10()
              p
            } else {
              p <- ggplot(plot_df, aes(x = "", y = .data[[v_name]])) +
                geom_boxplot(fill = "#f39c12", color = "#e67e22", alpha = 0.7,
                             outlier.color = "red", outlier.size = 2.5, na.rm = TRUE) +
                geom_jitter(width = 0.15, alpha = 0.25, size = 1, na.rm = TRUE) +
                theme_minimal() +
                theme(axis.title.x = element_blank(),
                      axis.text.x  = element_blank(),
                      axis.ticks.x = element_blank()) +
                labs(y = v_name)
              if (use_log) p <- p + scale_y_log10()
              p
            }
          })
        })
      } else {
        groups <- na.omit(unique(df[[input$group_var]]))
        for (g in groups) {
          local({
            v_name <- v
            g_name <- g
            plot_id <- paste0("plot_", make.names(v_name), "_", make.names(g_name))
            
            output[[plot_id]] <- renderPlot({
              req(v_name %in% names(df))
              plot_df <- df[df[[input$group_var]] == g_name, , drop = FALSE]
              
              if (isTRUE(input$plot_drop_outliers)) {
                keep_vals <- drop_outliers_iqr(plot_df[[v_name]])
                plot_df <- plot_df[plot_df[[v_name]] %in% keep_vals | is.na(plot_df[[v_name]]), , drop = FALSE]
              }
              
              use_log <- isTRUE(input$plot_log) && all(plot_df[[v_name]] > 0, na.rm = TRUE)
              
              if (input$plot_type == "dist") {
                p <- ggplot(plot_df, aes(x = .data[[v_name]])) +
                  geom_density(fill = "#00a65a", alpha = 0.4, color = "#008d4c", linewidth = 1, na.rm = TRUE) +
                  theme_minimal() +
                  labs(x = v_name, y = "Densité")
                if (use_log) p <- p + scale_x_log10()
                p
              } else {
                p <- ggplot(plot_df, aes(x = "", y = .data[[v_name]])) +
                  geom_boxplot(fill = "#00a65a", alpha = 0.5,
                               outlier.color = "red", outlier.size = 2.5, na.rm = TRUE) +
                  geom_jitter(width = 0.15, alpha = 0.25, size = 1, na.rm = TRUE) +
                  theme_minimal() +
                  theme(axis.title.x = element_blank(),
                        axis.text.x  = element_blank(),
                        axis.ticks.x = element_blank()) +
                  labs(y = paste0(v_name, " (", g_name, ")"))
                if (use_log) p <- p + scale_y_log10()
                p
              }
            })
          })
        }
      }
    }
  })
  
  corr_matrix <- reactive({
    df <- raw_data()
    req(df, input$target_vars, length(input$target_vars) > 1)
    
    num_df <- df %>%
      select(all_of(input$target_vars)) %>%
      select(where(is.numeric))
    
    req(ncol(num_df) > 1)
    round(cor(num_df, use = input$corr_use, method = input$corr_method), 2)
  })
  
  output$corr_plot <- renderPlot({
    cormat <- corr_matrix()
    cormat_melted <- as.data.frame(as.table(cormat))
    names(cormat_melted) <- c("Var1", "Var2", "Correlation")
    
    ggplot(cormat_melted, aes(x = Var1, y = Var2, fill = Correlation)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "#d9534f", high = "#3c8dbc", mid = "white",
                           midpoint = 0, limit = c(-1, 1), space = "Lab",
                           name = "Corrélation") +
      geom_text(aes(label = Correlation), color = "black", size = 5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
            axis.text.y = element_text(size = 12),
            axis.title = element_blank(),
            panel.grid.major = element_blank())
  })
  
  output$data_status <- renderValueBox({
    df <- raw_data()
    if (is.null(df)) {
      valueBox("En attente", "Téléversez des données", icon = icon("upload"), color = "red")
    } else {
      valueBox(paste0(nrow(df), " lignes / ", ncol(df), " colonnes"),
               "Données actives", icon = icon("check-circle"), color = "green")
    }
  })
  
  output$raw_table <- renderDT({
    df <- raw_data(); req(df)
    datatable(df, options = list(pageLength = 8, scrollX = TRUE), filter = "top")
  })
}

shinyApp(ui, server)