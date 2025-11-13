# Resumen del Proyecto: Análisis de Procesamiento de Lenguaje Natural del Guion de GTA San Andreas

## 📋 Índice
1. [Descripción General](#descripción-general)
2. [Objetivos del Proyecto](#objetivos-del-proyecto)
3. [Tecnologías Utilizadas](#tecnologías-utilizadas)
4. [Estructura del Análisis](#estructura-del-análisis)
5. [Componentes Principales](#componentes-principales)
6. [Resultados Visuales](#resultados-visuales)
7. [Insights Principales](#insights-principales)
8. [Cómo Usar el Proyecto](#cómo-usar-el-proyecto)

---

## 📖 Descripción General

Este proyecto aplica técnicas de **Procesamiento de Lenguaje Natural (NLP)** al guion completo del videojuego Grand Theft Auto: San Andreas. El análisis extrae patrones lingüísticos, relaciones entre personajes, y visualiza las interacciones narrativas mediante gráficos interactivos y análisis de redes sociales.

**Archivo de datos**: `guionGTA.txt` (433 KB con más de 10,000 líneas de diálogo)

**Documento principal**: `analisis_gta.Rmd` (RMarkdown con 1,161 líneas de código)

**Salida HTML**: `analisis_gta.html` (5.9 MB autocontenido con todas las visualizaciones)

---

## 🎯 Objetivos del Proyecto

### Objetivo General
Analizar el guion de GTA San Andreas utilizando técnicas de NLP para descubrir patrones narrativos, relaciones entre personajes y características lingüísticas del juego.

### Objetivos Específicos
1. **Análisis de Frecuencia**: Identificar las palabras más comunes en el guion
2. **Análisis de Bigramas**: Descubrir pares de palabras que aparecen juntas frecuentemente
3. **Modelado de Skip-grams**: Entender el contexto de las palabras mediante ventanas de 2 palabras
4. **Análisis de Redes de Personajes**: Visualizar interacciones entre los 13 personajes principales
5. **Métricas de Centralidad**: Determinar qué personajes son más importantes en la narrativa
6. **Clustering y Cohesión**: Evaluar la estructura comunitaria de las relaciones

---

## 🛠️ Tecnologías Utilizadas

### Lenguaje y Entorno
- **R versión 4.5.2**
- **RMarkdown**: Para crear documentos reproducibles con código y narrativa
- **RStudio**: IDE para desarrollo

### Paquetes de R

#### Análisis de Texto
```r
library(tidyverse)      # Manipulación de datos (dplyr, ggplot2, etc.)
library(tidytext)       # Análisis de texto tidy
library(stringr)        # Manipulación de strings
```

#### Visualización
```r
library(ggplot2)        # Gráficos estáticos
library(wordcloud)      # Nubes de palabras
library(networkD3)      # Gráficos de red interactivos
library(htmltools)      # Generación de HTML
library(kableExtra)     # Tablas HTML estilizadas
```

#### Análisis de Redes
```r
library(igraph)         # Análisis y métricas de redes sociales
library(pheatmap)       # Mapas de calor (heatmaps)
```

#### Estadística
```r
library(boot)           # Bootstrap para intervalos de confianza
library(purrr)          # Programación funcional
```

---

## 📊 Estructura del Análisis

El documento está organizado en las siguientes secciones:

### 1. **Introducción y Contexto** (Líneas 1-56)
- Presentación del proyecto
- Objetivos
- **Galería de Personajes**: 14 fotos con nombres de los protagonistas principales

### 2. **Configuración Inicial** (Líneas 57-125)
- Carga de librerías
- Configuración de tema visual
- Parámetros de gráficos

### 3. **Carga y Preprocesamiento** (Líneas 126-250)
- Lectura del guion desde `guionGTA.txt`
- **Unificación de personajes**:
  - `CARL` → `CJ` (mismo personaje)
  - `SMOKE` → `BIG SMOKE`
  - `DOGG` → `MADD DOGG`
- Tokenización de palabras
- Eliminación de stop words (palabras comunes sin significado)

### 4. **Análisis de Frecuencias** (Líneas 251-400)
- Top 20 palabras más frecuentes
- Gráfico de barras con frecuencias
- Nube de palabras (wordcloud)

### 5. **Análisis de Bigramas** (Líneas 401-580)
- Extracción de pares de palabras consecutivas
- Filtrado de bigramas significativos
- Visualización de top 15 bigramas
- Gráfico de red de bigramas

### 6. **Análisis de Skip-grams** (Líneas 581-740)
- Modelado de contexto con ventana de 2 palabras
- Pares de palabras que aparecen juntas (no necesariamente consecutivas)
- **Red interactiva de skip-grams** con networkD3
- Etiquetas permanentes en nodos

### 7. **Análisis de Personajes** (Líneas 741-950)
- Lista de 13 personajes principales:
  1. CJ (protagonista)
  2. SWEET
  3. BIG SMOKE
  4. RYDER
  5. KENDL
  6. CESAR
  7. CATALINA
  8. WOOZIE
  9. TENPENNY (antagonista)
  10. PULASKI
  11. TRUTH
  12. TORENO
  13. MADD DOGG
  14. ZERO

### 8. **Matriz de Interacciones** (Líneas 951-1050)
- **Tabla de adyacencia**: Muestra cuántas veces cada par de personajes aparece en la misma escena
- Estilo Bootstrap con scroll horizontal
- Exportación a `character_interactions_adjacency.csv`

### 9. **Red de Interacciones de Personajes** (Líneas 1051-1150)
- **Gráfico interactivo** usando forceNetwork de networkD3
- Configuración:
  - `linkDistance = 150`: Distancia entre nodos
  - `charge = -500`: Fuerza de repulsión
  - `fontSize = 18`: Tamaño de etiquetas
  - `opacityNoHover = 1`: Etiquetas siempre visibles
- Cada línea conecta personajes que interactúan
- Grosor de líneas = intensidad de interacción

### 10. **Métricas de Centralidad** (Líneas 1151-1161)
- **Degree (Grado)**: Número de conexiones directas
- **Betweenness (Intermediación)**: Qué tan frecuentemente un personaje está en el camino entre otros
- **Closeness (Cercanía)**: Qué tan cerca está de todos los demás
- **Eigenvector**: Importancia basada en la importancia de sus conexiones

---

## 🔧 Componentes Principales

### 1. Galería de Personajes (Líneas 57-125)

**Qué hace**: Muestra 14 imágenes de los personajes principales con sus nombres.

**Código clave**:
```r
htmltools::tags$div(
  style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); 
           gap: 20px; margin: 30px 0;",
  
  # CJ
  htmltools::tags$div(
    htmltools::tags$img(src = "images/characters/cj.png", 
                        style = "width: 100%; border-radius: 50%;"),
    htmltools::tags$p("CJ", style = "text-align: center; font-weight: bold;")
  ),
  # ... más personajes
)
```

**Por qué es importante**: Da contexto visual antes del análisis técnico.

---

### 2. Preprocesamiento del Texto

**Paso 1: Unificación de nombres**
```r
gta_script_raw <- str_replace_all(gta_script_raw, "\\bCARL\\b", "CJ")
gta_script_raw <- str_replace_all(gta_script_raw, "\\bSMOKE\\b", "BIG SMOKE")
gta_script_raw <- str_replace_all(gta_script_raw, "\\bDOGG\\b", "MADD DOGG")
```

**Paso 2: Tokenización**
```r
gta_tokens <- gta_script %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
```

**Resultado**: Texto limpio listo para análisis.

---

### 3. Detección de Interacciones entre Personajes

**Función personalizada**:
```r
find_character_mentions <- function(text, characters) {
  mentions <- sapply(characters, function(char) {
    str_detect(text, regex(paste0("\\b", char, "\\b"), ignore_case = TRUE))
  })
  characters[mentions]
}
```

**Lógica**:
1. Lee cada línea del guion
2. Detecta qué personajes aparecen en esa línea
3. Si 2+ personajes aparecen → crea una interacción
4. Cuenta interacciones para construir matriz

**Ejemplo de salida** (`character_interactions_adjacency.csv`):
```
personaje,CJ,SWEET,CESAR,KENDL,...
CJ,0,48,42,9,...
SWEET,48,0,15,8,...
CESAR,42,15,0,25,...
```

---

### 4. Visualizaciones Interactivas con networkD3

**Red de Skip-grams**:
```r
forceNetwork(
  Links = skipgram_graph_d3$links,
  Nodes = skipgram_graph_d3$nodes,
  Source = "source",
  Target = "target",
  Value = "n",
  NodeID = "name",
  Group = "group",
  linkDistance = 90,
  charge = -350,
  fontSize = 16,
  opacityNoHover = 1,  # Etiquetas siempre visibles
  zoom = TRUE
)
```

**Parámetros explicados**:
- `linkDistance`: Separación entre nodos (más alto = más separado)
- `charge`: Fuerza de repulsión (negativo = se repelen)
- `fontSize`: Tamaño del texto de las etiquetas
- `opacityNoHover = 1`: Las etiquetas no se desvanecen
- `zoom = TRUE`: Permite hacer zoom y pan

---

### 5. Tabla de Matriz con kableExtra

```r
character_adjacency_matrix %>%
  kable(format = "html", 
        caption = "Matriz de Adyacencia de Interacciones entre Personajes") %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed", "responsive"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  column_spec(1, bold = TRUE, border_right = TRUE) %>%
  scroll_box(width = "100%", height = "500px")
```

**Características**:
- **striped**: Filas alternadas en color
- **hover**: Resalta fila al pasar el mouse
- **condensed**: Compacta el espaciado
- **responsive**: Se adapta a diferentes pantallas
- **scroll_box**: Tabla desplazable (500px de alto)

---

## 📈 Resultados Visuales

### 1. Nube de Palabras
- **Tamaño** proporcional a frecuencia
- **Colores** aleatorios para estética
- Muestra las ~50 palabras más frecuentes

### 2. Gráfico de Bigramas
- Top 15 pares de palabras consecutivas
- Ordenado de mayor a menor frecuencia
- Barras horizontales con colores degradados

### 3. Red Interactiva de Skip-grams
- **Nodos**: Palabras individuales
- **Enlaces**: Pares que aparecen juntos
- **Grosor de línea**: Frecuencia de co-ocurrencia
- Interactivo: arrastrar, zoom, hover

### 4. Red de Personajes
- **Nodos**: Los 13 personajes principales
- **Enlaces**: Interacciones en escenas
- **Grosor**: Número de co-apariciones
- Permite ver quién interactúa más con quién

### 5. Mapa de Calor (Heatmap)
- Matriz de adyacencia visualizada con colores
- **Rojo oscuro**: Muchas interacciones
- **Amarillo claro**: Pocas interacciones
- Simétrica (CJ-SWEET = SWEET-CJ)

---

## 💡 Insights Principales

### Sobre el Lenguaje
1. **Palabras más comunes**: Reflejan el contexto urbano y de pandillas
2. **Bigramas frecuentes**: Frases características del juego
3. **Skip-grams**: Revelan contextos semánticos más amplios

### Sobre los Personajes

#### CJ (Protagonista)
- **Mayor grado de centralidad**: Interactúa con todos
- **Alta intermediación**: Conecta diferentes grupos
- Personaje central en la narrativa

#### SWEET (Hermano de CJ)
- Segunda mayor interacción con CJ (48 escenas juntos)
- Representa la familia Grove Street

#### BIG SMOKE (Antagonista secundario)
- Alta interacción temprana con CJ
- Conexiones con múltiples personajes

#### TENPENNY (Antagonista principal)
- Interacciones estratégicas clave
- Alta betweenness (intermediación)

### Sobre la Red
- **Densidad**: Indica qué tan conectada está la red de personajes
- **Diámetro**: Distancia máxima entre dos personajes
- **Clustering**: Formación de grupos (familia, policías, rivales)

---

## 🚀 Cómo Usar el Proyecto

### Requisitos Previos
```r
# Instalar paquetes necesarios
install.packages(c("tidyverse", "tidytext", "igraph", "networkD3", 
                   "kableExtra", "htmltools", "wordcloud", "boot", 
                   "purrr", "pheatmap"))
```

### Paso 1: Abrir el Proyecto
1. Abrir RStudio
2. Abrir el archivo `analisis_gta.Rmd`

### Paso 2: Renderizar el Documento
```r
# Opción 1: Usando el botón "Knit" en RStudio
# Opción 2: Desde la consola
rmarkdown::render("analisis_gta.Rmd")
```

### Paso 3: Ver el Resultado
- Se generará `analisis_gta.html`
- Abrir en cualquier navegador web
- Todas las visualizaciones son interactivas

### Estructura de Archivos
```
proyecto/
├── analisis_gta.Rmd              # Código fuente principal
├── analisis_gta.html             # Documento generado
├── guionGTA.txt                  # Datos del guion
├── character_interactions_adjacency.csv  # Matriz exportada
├── interactive_skipgram.html     # Widget de red
├── interactive_character_interactions.html  # Widget de personajes
└── images/
    └── characters/               # 14 fotos de personajes
        ├── cj.png
        ├── sweet.webp
        ├── BigSmoke.jpg
        └── ... (11 más)
```

---

## 📊 Métricas de Red Explicadas

### 1. Degree (Grado)
**Definición**: Número de conexiones directas que tiene un nodo.

**Fórmula**: $d(v) = \sum_{u \in V} A_{vu}$

**Interpretación**: Un personaje con alto grado interactúa con muchos otros personajes.

**Ejemplo**: Si CJ aparece en escenas con 12 personajes diferentes, su grado es 12.

---

### 2. Betweenness Centrality (Centralidad de Intermediación)
**Definición**: Mide cuántas veces un nodo está en el camino más corto entre otros dos nodos.

**Fórmula**: $C_B(v) = \sum_{s \neq v \neq t} \frac{\sigma_{st}(v)}{\sigma_{st}}$

Donde:
- $\sigma_{st}$ = número total de caminos más cortos entre s y t
- $\sigma_{st}(v)$ = número de esos caminos que pasan por v

**Interpretación**: Personajes "puente" que conectan diferentes grupos de la historia.

**Ejemplo**: CJ conecta a su familia (SWEET, KENDL) con otros grupos (WOOZIE, CESAR).

---

### 3. Closeness Centrality (Centralidad de Cercanía)
**Definición**: Inverso de la suma de distancias más cortas a todos los demás nodos.

**Fórmula**: $C_C(v) = \frac{n-1}{\sum_{u \neq v} d(v,u)}$

**Interpretación**: Qué tan "cerca" está un personaje de todos los demás en la red.

**Ejemplo**: Un personaje central puede "llegar" a cualquier otro en pocos pasos.

---

### 4. Eigenvector Centrality (Centralidad de Vector Propio)
**Definición**: Un nodo es importante si está conectado a nodos importantes.

**Fórmula**: $x_v = \frac{1}{\lambda} \sum_{u \in N(v)} x_u$

**Interpretación**: Importancia basada en la calidad de las conexiones, no solo cantidad.

**Ejemplo**: Estar conectado a CJ (central) da más importancia que estar conectado a ZERO (periférico).

---

### 5. Densidad de la Red
**Definición**: Proporción de conexiones existentes sobre todas las posibles.

**Fórmula**: $D = \frac{2|E|}{|V|(|V|-1)}$

Donde:
- $|E|$ = número de enlaces
- $|V|$ = número de nodos

**Interpretación**: Qué tan interconectada está la red (0 = sin conexiones, 1 = todos conectados).

---

### 6. Coeficiente de Clustering
**Definición**: Probabilidad de que dos vecinos de un nodo estén conectados entre sí.

**Fórmula**: $C_i = \frac{2e_i}{k_i(k_i-1)}$

Donde:
- $e_i$ = número de conexiones entre vecinos de i
- $k_i$ = grado del nodo i

**Interpretación**: Formación de "triángulos" o grupos cerrados.

**Ejemplo**: Si SWEET, CJ y KENDL todos interactúan entre sí → alto clustering (familia).

---

## 🎓 Conceptos de NLP Utilizados

### 1. Tokenización
**Definición**: Dividir texto en unidades individuales (palabras).

**Ejemplo**:
```
Texto: "CJ meets Big Smoke"
Tokens: ["CJ", "meets", "Big", "Smoke"]
```

### 2. Stop Words
**Definición**: Palabras comunes sin significado analítico (el, la, de, y, etc.).

**Acción**: Se eliminan para enfocarse en palabras significativas.

### 3. Bigramas
**Definición**: Pares de palabras consecutivas.

**Ejemplo**:
```
Texto: "Grove Street Families"
Bigramas: ["Grove Street", "Street Families"]
```

### 4. Skip-grams
**Definición**: Pares de palabras dentro de una ventana, sin necesidad de ser consecutivas.

**Ejemplo** (ventana = 2):
```
Texto: "CJ drives to Grove Street"
Skip-grams: 
  - (CJ, drives)
  - (CJ, to)
  - (drives, to)
  - (drives, Grove)
  - (to, Grove)
  - (to, Street)
```

### 5. TF-IDF (Term Frequency-Inverse Document Frequency)
**Nota**: No usado en este proyecto, pero útil para análisis más avanzados.

**Definición**: Mide la importancia de una palabra en un documento relativo a un corpus.

---

## 🔍 Preguntas Frecuentes para la Exposición

### ¿Por qué R y no Python?
- **R**: Excelente para análisis estadístico y visualización
- **tidytext**: Paquete específico para text mining en estilo "tidy"
- **RMarkdown**: Integración perfecta de código, narrativa y resultados

### ¿Cómo se detectan las interacciones entre personajes?
1. Se lee cada línea del guion
2. Se buscan nombres de personajes usando expresiones regulares
3. Si 2+ personajes aparecen en la misma línea → interacción
4. Se cuenta y se construye una matriz de adyacencia

### ¿Por qué networkD3 en lugar de gráficos estáticos?
- **Interactividad**: Permite explorar la red dinámicamente
- **Zoom y Pan**: Ver detalles sin perder el contexto general
- **Tooltips**: Información al pasar el mouse
- **Drag & Drop**: Reorganizar nodos para mejor visualización

### ¿Qué significa que las etiquetas tengan `opacityNoHover = 1`?
- Sin este parámetro, las etiquetas solo aparecen al pasar el mouse
- Con `opacityNoHover = 1`, las etiquetas están **siempre visibles**
- Mejora la legibilidad y facilita la presentación

### ¿Cómo se unificaron los nombres de personajes?
```r
# Antes: CARL y CJ aparecían como separados
# Después: Todo se convierte a CJ
gta_script_raw <- str_replace_all(gta_script_raw, "\\bCARL\\b", "CJ")

# Similar para BIG SMOKE/SMOKE y MADD DOGG/DOGG
```

**Razón**: Son el mismo personaje con diferentes nombres en el guion.

---

## 📝 Conclusiones

### Logros del Proyecto
1. ✅ Análisis completo de un guion de ~10,000 líneas
2. ✅ Identificación de 13 personajes principales
3. ✅ Creación de 6 visualizaciones diferentes (estáticas e interactivas)
4. ✅ Cálculo de métricas de red social
5. ✅ Documento HTML autocontenido de 5.9 MB
6. ✅ Exportación de datos a CSV para análisis posterior

### Aplicaciones Prácticas
- **Game Design**: Entender estructuras narrativas efectivas
- **Análisis de Guiones**: Evaluar balance entre personajes
- **Detección de Protagonistas**: Identificar roles principales automáticamente
- **Social Network Analysis**: Aplicable a cualquier narrativa (libros, películas, series)

### Posibles Extensiones
1. **Análisis de Sentimientos**: Detectar emociones en diálogos
2. **Topic Modeling**: Identificar temas recurrentes
3. **Análisis Temporal**: Cómo evolucionan las relaciones durante el juego
4. **Comparación con otros juegos**: GTA III, Vice City, GTA V
5. **Named Entity Recognition**: Detectar automáticamente nombres de personajes

---

## 📚 Referencias y Recursos

### Paquetes de R
- **tidytext**: Silge, J., & Robinson, D. (2016). tidytext: Text Mining and Analysis Using Tidy Data Principles in R
- **igraph**: Csardi, G., & Nepusz, T. (2006). The igraph software package for complex network research
- **networkD3**: Gandrud, C. (2016). networkD3: D3 JavaScript Network Graphs from R

### Conceptos de Redes
- Newman, M. E. J. (2018). *Networks: An Introduction*. Oxford University Press.
- Barabási, A.-L. (2016). *Network Science*. Cambridge University Press.

### NLP en R
- Silge, J., & Robinson, D. (2017). *Text Mining with R: A Tidy Approach*. O'Reilly Media.

---

## 🎤 Tips para la Exposición

### Orden Sugerido de Presentación
1. **Introducción** (2 min)
   - ¿Qué es GTA San Andreas?
   - ¿Por qué analizar su guion?

2. **Metodología** (3 min)
   - Herramientas: R, RMarkdown, networkD3
   - Proceso: Carga → Preprocesamiento → Análisis → Visualización

3. **Galería de Personajes** (1 min)
   - Mostrar las fotos
   - Explicar los 13 personajes principales

4. **Análisis de Palabras** (3 min)
   - Mostrar nube de palabras
   - Explicar bigramas y skip-grams
   - Red interactiva de skip-grams (¡hacer zoom en vivo!)

5. **Análisis de Personajes** (5 min)
   - Mostrar matriz de adyacencia
   - Red interactiva de personajes (¡arrastrar nodos!)
   - Explicar métricas: degree, betweenness, closeness

6. **Resultados e Insights** (3 min)
   - ¿Quién es el personaje más central? (CJ)
   - ¿Qué grupos se forman? (Familia, policías, aliados)
   - ¿Qué patrones lingüísticos aparecen?

7. **Conclusiones y Aplicaciones** (2 min)
   - Utilidad para game design
   - Aplicable a otras narrativas
   - Posibles extensiones

8. **Demo en Vivo** (1 min)
   - Abrir el HTML
   - Interactuar con las redes
   - Responder preguntas

### Puntos Clave a Enfatizar
- ✨ **Interactividad**: Las redes se pueden manipular en tiempo real
- 📊 **Datos reales**: 433 KB de guion original del juego
- 🔬 **Reproducibilidad**: Todo el código está disponible
- 🎮 **Aplicación práctica**: Útil para análisis de narrativas en videojuegos

### Preguntas que Pueden Hacer
**P: ¿Cuánto tiempo tomó el proyecto?**
R: El análisis completo se renderiza en ~30-60 segundos en una computadora moderna.

**P: ¿Por qué hay más interacciones CJ-SWEET?**
R: Son hermanos y aparecen juntos en muchas misiones del juego, especialmente al inicio.

**P: ¿Se podría hacer esto con otros juegos?**
R: ¡Sí! Solo se necesita el guion en formato de texto.

**P: ¿Qué significa que la red sea "scale-free"?**
R: Algunos nodos (como CJ) tienen muchas conexiones, mientras que la mayoría tienen pocas (ley de potencia).

---

## ✅ Checklist para la Exposición

### Antes de Presentar
- [ ] Revisar que `analisis_gta.html` se abra correctamente
- [ ] Probar las visualizaciones interactivas (zoom, drag, hover)
- [ ] Preparar el proyector/pantalla
- [ ] Tener RStudio abierto con el código fuente
- [ ] Verificar que las imágenes de personajes se vean bien
- [ ] Practicar la navegación del documento HTML

### Durante la Presentación
- [ ] Mostrar primero la galería de personajes (contexto visual)
- [ ] Explicar el preprocesamiento (unificación de nombres)
- [ ] Demostrar interactividad de las redes (¡impresiona!)
- [ ] Conectar métricas con la historia del juego
- [ ] Mencionar aplicaciones prácticas

### Después de la Presentación
- [ ] Compartir el HTML con la audiencia
- [ ] Ofrecer el código fuente en GitHub
- [ ] Responder preguntas técnicas
- [ ] Recopilar feedback para mejoras

---

## 🎯 Mensaje Final

Este proyecto demuestra cómo técnicas de **Data Science** y **NLP** pueden revelar patrones ocultos en narrativas complejas. Al aplicar análisis de redes sociales a un guion de videojuego, transformamos diálogos en **insights cuantificables** sobre estructura narrativa y desarrollo de personajes.

**Impacto**: Este tipo de análisis puede ayudar a guionistas, diseñadores de juegos y productores a crear narrativas más balanceadas y personajes mejor desarrollados.

---

*Documento creado para exposición del proyecto de NLP aplicado a GTA San Andreas*  
*Fecha: 13 de Noviembre de 2025*  
*Versión: 1.0*
