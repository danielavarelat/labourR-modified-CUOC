
<!-- README.md is generated from README.Rmd. Please edit that file -->

# labourR ORIGINAL 

The goal of labourR is to map multilingual free-text of occupations,
such as a job title in a Curriculum Vitae, to hierarchical ontologies
provided by [ESCO](https://ec.europa.eu/esco/portal), the multilingual
classification of European Skills, Competences, Qualifications and
Occupations, and
[ISCO](https://ec.europa.eu/esco/portal/escopedia/International_Standard_Classification_of_Occupations__40_ISCO_41_),
the International Standard Classification of Occupations.

<img src="man/figures/ESCO_ISCO_hierarchy.png" title="Fig.1 - ESCO is mapped to the 4th level of the ISCO hierarchical model." alt="Fig.1 - ESCO is mapped to the 4th level of the ISCO hierarchical model." width="80%" />

Computations are vectorised and the `data.table` package is used for
high performance and memory efficiency.

Ir al 
[Repositorio](https://eworx-org.github.io/labourR/articles/occupations_retrieval.html)
para más detalles.

------------------------------------------------------------------------------


# Nuestra implementación

El documento que se usa como base en este caso es la [Clasificación Única de Ocupaciones para Colombia – CUOC 2022](https://www.dane.gov.co/files/sen/nomenclatura/cuoc/documento-clasificacion-unica-ocupaciones-colombia-CUOC-2022.pdf). 


Como clasificación inicia se tienen los 10 GRANDES GRUPOS, cuyo código únicamente tiene un dígito. De ahí en adelante, se tienen SUBGRUPOS PRINCIPALES (dos dígitos), Subgrupos (tres dígitos), Grupos primarios (cuatro dígitos) y finalmente las Ocupaciones (cinco dígitos). 


<img width="521" alt="foto" src="https://github.com/danielavarelat/labourR-modified-CUOC/assets/47607161/d4db5a70-8a08-41a9-9509-8939b023e3b5">


### Metodología 

El corpus de entrada o la vacante individual debe ser limpiada y tokenizado: hay funciones ya para esto, pero no se corren dentro de la función ppal; es un paso de pre-procesamiento previo.  
Parte de este preprocesamiento incluye concatenar las columnas que se quieren analizar,  por ejemplo: unir las palabras claves con el título y la descripción. 

Las tablas TFIDF se pueden construir con el código del repositorio principal (revisar arriba). Estas son la base necesaria para el funcionamiento del algoritmo. 

El valor agregado de esta implementación está en la utilización de dos niveles de clasificación para restringir el universo de posibilidades de ocupaciones. Primero, se trata de identificar si la vacante tiene palabras que sean "domain-specific" para los GRANDES GRUPOS y si no encuentra, hace TFIDF para este primer nivel. Una vez determina el primer nivel, solo busca el match usando TFIDF con las ocupaciones dentro de este NIVEL GENERAL. 


ARCHIVO UTILIZADO -> Correlativa_CUOC-2022_Vs_CNO-2022.xlsx

### Entradas 

- **corpus_one**: una vacante en forma de data.table

- **id_col, text_col1, text_col2**: Nombre de la columna de ID, el texto a usar en la primera clasficación y en la segunda (pueden ser los mismos o puede ser por ejemplo título y palabras claves para lo primero, y después todo junto incluyendo la descripción). 

- **num_leaves_final**: Número de ocupaciones a predecir para cada vacante.

Las siguientes tablas son entradas que deben construirse previamente 

- **vocabulary_domain**: tabla con los términos que son específicos para cada uno de los 10 GRANDES GRUPOS. | term | levels |
  En este caso son 697.

- **table_tfidf_broad**: tabla TFIDF construida usando las "Denominaciones CUOC 2022" del archivo fuente para cada uno de los GRANDES GRUPOS.
<img width="1445" alt="imagen" src="https://github.com/danielavarelat/labourR-modified-CUOC/assets/47607161/67021422-89c2-4db4-a325-ff0187e7079e">

- **table_tfidf_granular**: tabla TFIDF construida usando las "Ocupaciones" y "Descripciones" para el nivel de Ocupaciones. 
<img width="1414" alt="imagen" src="https://github.com/danielavarelat/labourR-modified-CUOC/assets/47607161/7ceba321-4ecf-48ab-b4ee-13a46b5ba379">
<img width="1195" alt="imagen" src="https://github.com/danielavarelat/labourR-modified-CUOC/assets/47607161/d1c58079-2de1-4b87-a8ef-ca88c2e8e78b">


    corpus_in <- data.frame(
                id = 1,
                text = c("texto de la vacante"))
  
    single_two_steps(corpus_one = corpus_in, 
                     vocabulary_domain = vocabulary_domain, 
                     table_tfidf_broad = level1_tfidf,
                     table_tfidf_granular=granular_tfidf,
                     id_col='id', 
                     text_col1='text', 
                     text_col2='text',
                     num_leaves_final = 3)



