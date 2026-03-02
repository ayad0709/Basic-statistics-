# Fichier : app.R
# Objectif : Tableau de bord statistique ULTIME (Normalité, Outliers, Corrélations)
# Version : +++ démos (beaucoup), SANS boutons de download
# Ajout : définitions étendues FR + AR + nouvelles stats (P10/P90, trim, winsor, JB, NA%)
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

# --- 1) DICTIONNAIRE STATISTIQUE (FR + AR, paragraphes) ---
stat_dict <- list(
  "n" = list(
    desc = list(
      fr = "Le nombre d’observations valides correspond au total de valeurs réellement utilisées dans les calculs (après exclusion des NA). Il influence la stabilité des estimations : plus n est grand, plus les statistiques (moyenne, quantiles, corrélations) sont généralement fiables. n ne mesure pas la “qualité” des données, seulement leur quantité disponible.",
      ar = "يمثل عدد الملاحظات الصالحة إجمالي القيم المستخدمة فعليًا في الحسابات بعد استبعاد القيم المفقودة. كلما كبر n أصبحت الإحصاءات (المتوسط، الكوانتيلات، الارتباطات) أكثر استقرارًا وموثوقية. n لا يقيس جودة البيانات بل يوضح كمية القيم المتاحة."
    ),
    math = ""
  ),
  "n_na" = list(
    desc = list(
      fr = "Le nombre de valeurs manquantes (NA) indique combien d’observations sont absentes. Si les NA ne sont pas aléatoires (par ex. plus fréquents dans un groupe), ils peuvent biaiser l’analyse. En descriptif, on ignore souvent les NA, mais il est important de surveiller n_na pour décider : suppression, imputation, ou analyse du mécanisme de manque.",
      ar = "عدد القيم المفقودة (NA) يوضح كم ملاحظة غير متوفرة. إذا لم تكن القيم المفقودة عشوائية فقد تسبب تحيزًا (مثلًا: أكثر في مجموعة معينة). غالبًا تُتجاهل NA في الوصف، لكن متابعة n_na مهمة لاختيار استراتيجية: حذف، تعويض، أو دراسة سبب الفقد."
    ),
    math = ""
  ),
  "na_pct" = list(
    desc = list(
      fr = "Le pourcentage de valeurs manquantes (NA%) met n_na en contexte par rapport à la taille totale. Il permet de comparer la complétude entre variables. Un NA% élevé peut réduire la puissance statistique et rendre certaines comparaisons moins fiables, surtout si les valeurs manquantes se concentrent dans certaines catégories.",
      ar = "نسبة القيم المفقودة (NA%) تضع n_na في سياقه مقارنةً بالحجم الكلي، وتسمح بمقارنة اكتمال البيانات بين المتغيرات. ارتفاع NA% قد يقلل القوة الإحصائية ويضعف موثوقية المقارنات خاصةً إذا تركزت القيم المفقودة في فئات معينة."
    ),
    math = ""
  ),
  "sum" = list(
    desc = list(
      fr = "La somme est le total des valeurs valides. Elle est utile pour des variables additives (quantités, ventes, totaux) mais dépend fortement de n : si vous comparez des groupes de tailles différentes, la somme peut être trompeuse. On la complète souvent par la moyenne ou par des taux.",
      ar = "المجموع هو إجمالي القيم الصالحة. يفيد في المتغيرات التجميعية (كميات، مبيعات، إجماليات) لكنه يعتمد بشدة على n: مقارنة مجموعات بأحجام مختلفة قد تكون مضللة. غالبًا يُكمّل بالمتوسط أو بالمعدلات."
    ),
    math = ""
  ),
  "mean" = list(
    desc = list(
      fr = "La moyenne arithmétique résume les données par un centre (somme / n). Elle est informative pour des distributions symétriques, mais sensible aux valeurs extrêmes (outliers) qui peuvent la déplacer. Si la distribution est asymétrique, il est pertinent de regarder aussi la médiane et des mesures robustes (MAD/IQR), ainsi qu’un graphique.",
      ar = "المتوسط الحسابي يلخص البيانات بقيمة مركزية (المجموع / n). يكون مفيدًا للتوزيعات المتماثلة لكنه حساس للقيم المتطرفة التي قد تغيّره. إذا كان التوزيع منحرفًا فمن الأفضل النظر أيضًا إلى الوسيط ومقاييس مقاومة (MAD/IQR) مع رسم بياني."
    ),
    math = "\\[ \\bar{x} = \\frac{1}{n} \\sum_{i=1}^{n} x_i \\]"
  ),
  "mean_trim" = list(
    desc = list(
      fr = "La moyenne tronquée (trim) calcule la moyenne après avoir retiré une proportion des plus petites et des plus grandes valeurs (ici 10% à chaque extrémité). Elle réduit l’influence des outliers tout en gardant une logique proche de la moyenne. C’est un bon compromis quand on veut une moyenne plus robuste sans passer au tout-médiane.",
      ar = "المتوسط المُشذّب (Trimmed mean) يحسب المتوسط بعد حذف نسبة من أصغر وأكبر القيم (هنا 10% من كل طرف). يقلل تأثير القيم الشاذة مع الحفاظ على فكرة المتوسط. يعد حلًا وسطًا جيدًا عندما نريد متوسطًا أكثر مقاومة دون الاكتفاء بالوسيط."
    ),
    math = ""
  ),
  "winsor_mean" = list(
    desc = list(
      fr = "La moyenne winsorisée remplace (au lieu de supprimer) les valeurs extrêmes par des valeurs limites (ici aux quantiles 10% et 90%), puis calcule la moyenne. Elle stabilise la moyenne quand des extrêmes existent, tout en conservant n constant. Utile si vous ne voulez pas “perdre” d’observations.",
      ar = "المتوسط الوينسوري (Winsorized mean) يستبدل القيم المتطرفة بقيم حدّية (هنا عند كوانتيل 10% و90%) ثم يحسب المتوسط. يثبت المتوسط عند وجود قيم شاذة مع الحفاظ على n ثابتًا. مفيد إذا كنت لا تريد حذف ملاحظات."
    ),
    math = ""
  ),
  "ci_mean" = list(
    desc = list(
      fr = "L’intervalle de confiance (IC) à 95% de la moyenne fournit une plage plausible pour la moyenne vraie de la population, compte tenu de la variabilité et de n. Un IC étroit = estimation précise, un IC large = incertitude. Il est basé sur la loi de Student lorsque l’écart-type populationnel est inconnu. Pour des distributions très non normales et petits n, l’IC peut être moins fiable.",
      ar = "فترة الثقة 95% للمتوسط تعطي نطاقًا معقولًا لمتوسط المجتمع الحقيقي اعتمادًا على التباين وحجم العينة. النطاق الضيق يعني دقة أعلى، والواسع يعني عدم يقين أكبر. تُحسب غالبًا بتوزيع ستودنت عند عدم معرفة انحراف المجتمع. مع توزيعات غير طبيعية جدًا وn صغير قد تقل الموثوقية."
    ),
    math = "\\[ IC_{95\\%} = \\left[ \\bar{x} - t_{0.975} \\frac{s}{\\sqrt{n}} \\, ; \\, \\bar{x} + t_{0.975} \\frac{s}{\\sqrt{n}} \\right] \\]"
  ),
  "geom_mean" = list(
    desc = list(
      fr = "La moyenne géométrique convient aux phénomènes multiplicatifs (croissance, rendements). Elle revient à moyenner les logarithmes puis revenir à l’échelle d’origine. Elle n’est définie que pour des valeurs strictement positives (pas de 0/valeurs négatives).",
      ar = "المتوسط الهندسي مناسب للظواهر المضاعِفة (النمو، العوائد). يعادل متوسط اللوغاريتمات ثم العودة للمقياس الأصلي. لا يُعرّف إلا للقيم الموجبة فقط (لا أصفار ولا قيم سالبة)."
    ),
    math = "\\[ \\left(\\prod_{i=1}^n x_i\\right)^{\\frac{1}{n}} \\]"
  ),
  "harm_mean" = list(
    desc = list(
      fr = "La moyenne harmonique est pertinente pour moyenner des taux/سرعات et des ratios. Elle donne plus de poids aux petites valeurs. Elle exige des valeurs positives et peut devenir instable si certaines valeurs sont proches de zéro.",
      ar = "المتوسط التوافقي مناسب لمتوسط المعدلات/السرعات والنِّسَب ويعطي وزنًا أكبر للقيم الصغيرة. يتطلب قيماً موجبة وقد يصبح غير مستقر إذا اقتربت قيم من الصفر."
    ),
    math = "\\[ \\frac{n}{\\sum_{i=1}^n \\frac{1}{x_i}} \\]"
  ),
  "median" = list(
    desc = list(
      fr = "La médiane est la valeur centrale (50% en dessous / 50% au-dessus). Elle est robuste aux outliers et recommandée pour les distributions asymétriques. Elle se complète naturellement avec l’IQR pour décrire la dispersion des 50% centraux.",
      ar = "الوسيط هو القيمة المركزية (50% أقل و50% أكبر). وهو مقاوم للقيم الشاذة ويُفضّل في التوزيعات المنحرفة. غالبًا يُكمّل بـ IQR لوصف تشتت 50% الوسطى."
    ),
    math = ""
  ),
  "mode" = list(
    desc = list(
      fr = "Le mode est la valeur la plus fréquente. Très utile pour des variables discrètes/catégorielles. Pour des variables continues avec beaucoup de valeurs uniques, le mode “exact” peut être peu informatif ; un mode par classes (histogramme) est alors plus pertinent.",
      ar = "المنوال هو أكثر قيمة تكرارًا. مفيد جدًا للمتغيرات المنفصلة أو الفئوية. في المتغيرات المستمرة ذات القيم الفريدة الكثيرة قد لا يكون المنوال الدقيق ذا معنى؛ المنوال حسب الفئات عبر المدرج قد يكون أنسب."
    ),
    math = ""
  ),
  "sd" = list(
    desc = list(
      fr = "L’écart-type mesure la dispersion autour de la moyenne, dans نفس وحدة المتغير. Il est sensible aux extrêmes : quelques outliers peuvent l’augmenter. Pour des distributions très asymétriques, on peut préférer MAD/IQR pour une dispersion plus robuste.",
      ar = "الانحراف المعياري يقيس التشتت حول المتوسط وبنفس وحدة المتغير. وهو حساس للقيم المتطرفة وقد يرتفع بسببها. في التوزيعات المنحرفة جدًا قد تكون MAD أو IQR أكثر مقاومة لوصف التشتت."
    ),
    math = "\\[ s = \\sqrt{\\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2} \\]"
  ),
  "var" = list(
    desc = list(
      fr = "La variance est l’écart-type au carré. Elle est fondamentale dans beaucoup de formules (statistique inférentielle, modèles), mais moins intuitive car exprimée en unités carrées. On interprète souvent sd plutôt que var.",
      ar = "التباين هو مربع الانحراف المعياري. مهم في كثير من الصيغ والنماذج لكنه أقل سهولة في التفسير لأنه بوحدات مربعة. غالبًا يُفسَّر sd بدلًا من var."
    ),
    math = "\\[ s^2 = \\frac{1}{n-1} \\sum_{i=1}^{n} (x_i - \\bar{x})^2 \\]"
  ),
  "se" = list(
    desc = list(
      fr = "L’erreur standard (SE) quantifie l’incertitude sur la moyenne estimée : sd / √n. Elle diminue quand n augmente. Le SE décrit la précision de l’estimation, pas la dispersion des données (ça, c’est sd).",
      ar = "الخطأ المعياري (SE) يقيس عدم اليقين حول متوسط العينة: ‏sd / √n. ينخفض مع زيادة n. ‏SE يصف دقة التقدير وليس تشتت البيانات (التشتت هو sd)."
    ),
    math = "\\[ SE = \\frac{s}{\\sqrt{n}} \\]"
  ),
  "cv" = list(
    desc = list(
      fr = "Le coefficient de variation (CV%) est sd rapporté à la moyenne (×100). Il permet de comparer la variabilité entre variables d’échelles différentes. Si la moyenne est proche de 0, le CV peut devenir instable et perdre son sens.",
      ar = "معامل التباين (CV%) هو sd مقسومًا على المتوسط (×100). يفيد لمقارنة التشتت بين متغيرات بمقاييس مختلفة. إذا كان المتوسط قريبًا من الصفر قد يصبح CV غير مستقر ويفقد معناه."
    ),
    math = "\\[ CV = \\frac{s}{\\bar{x}} \\times 100 \\]"
  ),
  "mad" = list(
    desc = list(
      fr = "Le MAD (Median Absolute Deviation) mesure la dispersion autour de la médiane. Il est très robuste aux outliers et est souvent préférable à sd quand les données contiennent des valeurs extrêmes ou sont fortement asymétriques.",
      ar = "‏MAD يقيس التشتت حول الوسيط وهو مقاوم جدًا للقيم الشاذة. غالبًا يكون أفضل من sd عند وجود قيم متطرفة أو توزيع منحرف بشدة."
    ),
    math = "\\[ MAD = \\text{médiane}(|x_i - \\tilde{x}|) \\]"
  ),
  "iqr" = list(
    desc = list(
      fr = "L’IQR = Q3 − Q1 décrit la dispersion des 50% centraux et est robuste aux extrêmes. Il est utilisé dans les boxplots et dans la règle 1.5×IQR pour détecter les outliers.",
      ar = "‏IQR = Q3 − Q1 يصف تشتت 50% الوسطى وهو مقاوم للقيم المتطرفة. يُستخدم في مخطط الصندوق وقاعدة ‎1.5×IQR‎ لاكتشاف القيم الشاذة."
    ),
    math = "\\[ IQR = Q_3 - Q_1 \\]"
  ),
  "p10" = list(
    desc = list(
      fr = "Le décile 10% (P10) est la valeur en dessous de laquelle se trouvent 10% des observations. Il décrit le bas de la distribution de façon plus robuste que le minimum. Utile pour résumer la « queue basse » sans être dominé par un seul point extrême.",
      ar = "العُشير 10% (P10) هو القيمة التي يقع تحتها 10% من الملاحظات. يصف الطرف الأدنى للتوزيع بشكل أكثر مقاومة من الحد الأدنى. مفيد لتلخيص الذيل السفلي دون أن تهيمن قيمة متطرفة واحدة."
    ),
    math = ""
  ),
  "p90" = list(
    desc = list(
      fr = "Le décile 90% (P90) est la valeur en dessous de laquelle se trouvent 90% des observations (donc 10% au-dessus). Il décrit le haut de la distribution de manière robuste. Utile pour comprendre les niveaux élevés sans dépendre uniquement du maximum.",
      ar = "العُشير 90% (P90) هو القيمة التي يقع تحتها 90% من الملاحظات (أي 10% أعلى منها). يصف الطرف الأعلى للتوزيع بشكل مقاوم. مفيد لفهم المستويات العالية دون الاعتماد فقط على القيمة العظمى."
    ),
    math = ""
  ),
  "min" = list(
    desc = list(
      fr = "La valeur minimale est la plus petite observation (hors NA). Elle peut être affectée par une valeur aberrante, لذا تُقرأ مع P10/Q1 pour une image plus stable.",
      ar = "القيمة الصغرى هي أصغر ملاحظة (باستثناء NA). قد تتأثر بقيمة شاذة، لذا يُفضل قراءتها مع P10 وQ1 للحصول على صورة أكثر ثباتًا."
    ),
    math = ""
  ),
  "q1" = list(
    desc = list(
      fr = "Q1 (25%) : 25% des valeurs sont ≤ Q1. Il résume le bas de la distribution de façon robuste et sert à calculer l’IQR.",
      ar = "‏Q1 (25%) : ‏25% من القيم ≤ Q1. يصف الجزء الأدنى بشكل مقاوم ويستخدم لحساب IQR."
    ),
    math = ""
  ),
  "q3" = list(
    desc = list(
      fr = "Q3 (75%) : 75% des valeurs sont ≤ Q3. Avec Q1, il définit l’IQR et participe à la détection d’outliers.",
      ar = "‏Q3 (75%) : ‏75% من القيم ≤ Q3. مع Q1 يحدد IQR ويساعد في اكتشاف القيم الشاذة."
    ),
    math = ""
  ),
  "max" = list(
    desc = list(
      fr = "La valeur maximale est la plus grande observation (hors NA). Elle peut être dominée par un outlier ; comparez-la à P90/Q3 pour juger si elle est atypique.",
      ar = "القيمة العظمى هي أكبر ملاحظة (باستثناء NA). قد تهيمن عليها قيمة شاذة؛ قارنها بـ P90 وQ3 لتقييم مدى شذوذها."
    ),
    math = ""
  ),
  "range" = list(
    desc = list(
      fr = "L’étendue (max − min) résume la dispersion totale mais dépend uniquement des deux extrêmes, donc elle est très sensible aux outliers. À compléter par sd/IQR pour une vision plus robuste.",
      ar = "المدى (max − min) يلخص التشتت الكلي لكنه يعتمد فقط على الطرفين، لذا فهو حساس جدًا للقيم الشاذة. يُفضل دعمه بـ sd أو IQR لوصف أكثر مقاومة."
    ),
    math = "\\[ R = x_{\\max} - x_{\\min} \\]"
  ),
  "skew" = list(
    desc = list(
      fr = "La skewness mesure l’asymétrie : مثبتة à droite (positive) ou à gauche (négative). Une skewness proche de 0 suggère une distribution assez symétrique. Elle aide à choisir des transformations (log) ou des mesures robustes.",
      ar = "الالتواء (Skewness) يقيس عدم التماثل: موجب عند ذيل يميني أطول وسالب عند ذيل يساري أطول. قربه من الصفر يعني تماثلًا نسبيًا. يساعد على اختيار التحويلات (لوغاريتم) أو المقاييس المقاومة."
    ),
    math = "\\[ \\gamma_1 = \\frac{\\sum (x_i - \\bar{x})^3}{(n-1)s^3} \\]"
  ),
  "kurt" = list(
    desc = list(
      fr = "La kurtosis décrit la forme des queues et le pic. Des valeurs élevées suggèrent des queues plus lourdes (plus d’extrêmes). L’interprétation dépend de la convention (kurtosis vs excès). Ici, elle est surtout un indicateur d’extrêmes potentiels.",
      ar = "التفرطح (Kurtosis) يصف شكل الذيول وحدّة القمة. القيم الأعلى قد تعني ذيولًا أثقل (قيم متطرفة أكثر). يعتمد التفسير على التعريف المستخدم. هنا نستخدمه كمؤشر لاحتمال وجود قيم متطرفة."
    ),
    math = "\\[ K = \\frac{\\sum (x_i - \\bar{x})^4}{(n-1)s^4} \\]"
  ),
  "shapiro" = list(
    desc = list(
      fr = "Le test de Shapiro-Wilk évalue la normalité. Si p < 0.05, on rejette souvent la normalité. Attention : avec un grand n, de petites déviations deviennent significatives. Combinez toujours avec un graphique (densité/QQ-plot si vous en ajoutez). (Limité à n ≤ 5000 dans cette app.)",
      ar = "اختبار شابيرو-ويلك يختبر طبيعية التوزيع. إذا p < 0.05 نرفض غالبًا الطبيعية. تنبيه: مع n كبير قد تصبح انحرافات صغيرة ذات دلالة. يُفضل دائمًا دعمه برسم (كثافة/QQ إذا أضفته). (في هذا التطبيق: حتى n ≤ 5000)."
    ),
    math = "\\[ W = \\frac{\\left(\\sum a_i x_{(i)}\\right)^2}{\\sum(x_i - \\bar{x})^2} \\]"
  ),
  "jb" = list(
    desc = list(
      fr = "Le test de Jarque–Bera (p-value) évalue la normalité via la skewness et la kurtosis : une distribution normale a une asymétrie ~0 et une kurtosis ~3. Si p < 0.05, la normalité est souvent rejetée. Comme les autres tests, il est sensible à n : avec de grands échantillons, il détecte de petites déviations. Dans cette app, JB est calculé sans package externe (approximation chi-deux, ddl=2).",
      ar = "اختبار جاركي–بيرا (قيمة p) يختبر الطبيعية اعتمادًا على الالتواء والتفرطح: التوزيع الطبيعي له التواء ~0 وتفرطح ~3. إذا p < 0.05 غالبًا نرفض الطبيعية. مثل بقية الاختبارات هو حساس لحجم العينة: مع n كبير يلتقط انحرافات صغيرة. هنا يُحسب JB دون حزم إضافية (تقريب كاي-تربيع بدرجتي حرية)."
    ),
    math = ""
  ),
  "outliers" = list(
    desc = list(
      fr = "Détection d’outliers via la règle 1.5×IQR : une valeur est atypique si x < Q1 − 1.5×IQR ou x > Q3 + 1.5×IQR. Cette règle est robuste et correspond au boxplot. Un outlier n’est pas forcément une erreur : il peut refléter un phénomène réel mais doit être vérifié car il peut influencer moyenne/sd et certains modèles.",
      ar = "اكتشاف القيم الشاذة بقاعدة ‎1.5×IQR‎: تعتبر القيمة شاذة إذا x < Q1 − 1.5×IQR أو x > Q3 + 1.5×IQR. هذه القاعدة مقاومة وتوافق مخطط الصندوق. القيمة الشاذة ليست دائمًا خطأً لكنها تستحق التحقق لأنها قد تؤثر على المتوسط/الانحراف وبعض النماذج."
    ),
    math = "\\[ \\text{Outlier si } x < Q_1 - 1.5 \\times IQR \\text{ ou } x > Q_3 + 1.5 \\times IQR \\]"
  )
)

# --- 2) JEUX DE DONNÉES DE DÉMO (beaucoup) ---
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
  # 1) ggplot2 d'abord
  obj <- tryCatch(get(name, envir = asNamespace("ggplot2")), error = function(e) NULL)
  if (!is.null(obj)) return(as.data.frame(obj))
  
  # 2) datasets via data()
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
  
  # Options (labels) -> keys
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
    "Normalité (p-value Jarque–Bera)" = "jb",
    "Valeurs Aberrantes (N)" = "outliers"
  )
  
  observeEvent(input$show_settings, {
    showModal(modalDialog(
      title = "Sélectionner les statistiques à afficher",
      checkboxGroupInput(
        "selected_stats", "Choisissez les mesures :",
        choices = all_stats_options,
        selected = c("n","mean","median","sd")
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
  
  # Helper winsor
  winsorize_vec <- function(x, p = 0.10) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(x)
    lo <- as.numeric(quantile(x, p, names = FALSE))
    hi <- as.numeric(quantile(x, 1 - p, names = FALSE))
    pmin(pmax(x, lo), hi)
  }
  
  # Jarque-Bera p-value (sans package)
  jb_pvalue <- function(x) {
    x <- x[!is.na(x)]
    n <- length(x)
    if (n < 3) return(NA_real_)
    s <- moments::skewness(x)
    k <- moments::kurtosis(x)  # généralement kurtosis "non-excess"
    jb <- (n / 6) * (s^2 + ((k - 3)^2) / 4)
    1 - pchisq(jb, df = 2)
  }
  
  calculate_exhaustive_stats <- function(x) {
    n_total <- length(x)
    n_na <- sum(is.na(x))
    na_pct <- if (n_total > 0) (n_na / n_total) * 100 else NA_real_
    
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
    
    p10_val <- as.numeric(quantile(x_clean, 0.10, names = FALSE))
    p90_val <- as.numeric(quantile(x_clean, 0.90, names = FALSE))
    
    shapiro_p <- if (n >= 3 && n <= 5000) shapiro.test(x_clean)$p.value else NA_real_
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
      jb = jb_pvalue(x_clean),
      outliers = outliers_n
    )
  }
  
  output$exhaustive_stats_ui <- renderUI({
    df <- raw_data()
    active_stats <- if (is.null(input$selected_stats)) {
      c("n","mean","median","sd")
    } else input$selected_stats
    
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
          
          display_val <- if (is.null(val) || length(val) == 0 || is.na(val[1])) {
            "N/A"
          } else if (is.numeric(val)) {
            if (stat_key %in% c("shapiro","jb") && is.finite(val) && val < 0.001) "< 0.001"
            else if (stat_key == "na_pct") paste0(round(val, 2), "%")
            else round(val, 4)
          } else {
            val
          }
          
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
              if (!is.null(stat_dict[[stat_key]]$math) && stat_dict[[stat_key]]$math != "") {
                div(class = "math-text", stat_dict[[stat_key]]$math)
              } else NULL
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
        div(class="group-header", h3("Analyse Globale")),
        fluidRow(lapply(input$target_vars, function(v) build_card(df, v)))
      )
    } else {
      groups <- na.omit(unique(df[[input$group_var]]))
      tagList(lapply(groups, function(g) {
        tagList(
          div(class="group-header", h3(paste("Groupe :", g))),
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