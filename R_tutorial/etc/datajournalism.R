library(dplyr)

data <- c("7AAVDM12 Introduction to Data Journalism","King's College London","GB","Postgraduate","Module","https://www.kcl.ac.uk/artshums/depts/ddh/study/pgt/modules-2017-18/optional-modules/7AAVDM12.aspx",
        "Computational and Data Journalism (MSc)","Cardiff University","GB","Postgraduate","Programme","https://www.cardiff.ac.uk/study/postgraduate/taught/courses/course/computational-and-data-journalism-msc",
        "Data Journalism","NYU Journalism","US","Postgraduate","Module","https://journalism.nyu.edu/about-us/course/2016-spring/data-journalism-9/",
        "Data Journalism","University of Southern California","US","Undergraduate","Module","https://classes.usc.edu/term-20163/course/jour-322/",
        "Data Journalism (Interdisciplinary)","University of Missouri","US","Postgraduate","Programme","https://journalism.missouri.edu/programs/undergraduate/general/independent-study/data-journalism-interdisciplinary/",
        "Data Journalism (JOUR3000)","The University of Queensland","AU","Undergraduate","Module","https://www.uq.edu.au/study/course.html?course_code=JOUR3000",
        "Data Journalism Concentration","De Paul University","US","Postgraduate","Programme","https://www.depaul.edu/university-catalog/degree-requirements/graduate/communication/journalism-ma/concentrations/Pages/data-journalism.aspx",
        "Graduate Certificate in Data Journalism","George Mason University","US","Postgraduate","Certificate","https://cos.gmu.edu/ggs/academic-programs/graduate-certificate-in-data-journalism/",
        "JO2204: Data Journalism","City, University of London","GB","Postgraduate","Module","https://www.city.ac.uk/courses/postgraduate/interactive-journalism",
        "JOU 492 Data Journalism","Northern Kentucky University","US","Undergraduate","Module","http://onlinecatalog.nku.edu/preview_course_nopop.php?catoid=9&coid=19209",
        "JOU4210 Data Journalism","John F. Kennedy University","US","Undergraduate","Module","https://www.jfku.edu/Programs-and-Courses/College-of-Business-Professional-Studies/Journalism/Courses/JOU4210.html",
        "JOUR 499 - Data	Journalism","University of South Carolina","US","Undergraduate","Module","https://www.sc.edu/study/colleges_schools/cic/journalism_and_mass_communications/newsplex/training.php",
        "Jour 319: Data Journalism","University of Nevada","US","Undergraduate","Module","https://journalism.unr.edu/courses-2/data-journalism/",
        "JOUR 4790 - Data Journalism","Ohio University","US","Undergraduate","Module","http://www.catalogs.ohio.edu/preview_course_nopop.php?catoid=45&coid=216429",
        "JPW 301/Data Journalism","The College of New Jersey","US","Undergraduate","Module","https://journalism.tcnj.edu/course-descriptions/",
        "M.S. Data Journalism","Columbia University","US","Postgraduate","Programme","https://journalism.columbia.edu/ms-data-journalism",
        "MA in Data Journalism","Birmingham City University","GB","Postgraduate","Programme","http://www.bcu.ac.uk/media/courses/data-journalism-ma-2018-19",
        "Master en Periodismo de Investigacion, Datos y Visualizacion","University Rey Juan Carlos of Madrid","ES","Postgraduate","Programme","http://www.escuelaunidadeditorial.es/formacion-presencial/masteres/master-en-periodismo-de-investigacion-datos-y-visualizacion",
        "Master of Journalism in Data and Investigative Journalism","University of King's College","CA","Postgraduate","Programme","https://ukings.ca/area-of-study/master-of-journalism-data-investigative/",
        "MC 316 - Data Journalism","Kansas State University","US","Undergraduate","Module","https://catalog.k-state.edu/preview_course_nopop.php?catoid=36&coid=208428",
        "PMMA 6103 - Data Journalism and Interactive Graphics","Fordham University","US","Undergraduate","Module","https://www.fordham.edu/info/20968/ma_in_public_media_curriculum/2076/journalism_concentration",
        "Public Affairs Data Journalism I & II","Stanford University","US","Postgraduate","Module","http://journalism.stanford.edu/curriculum/",
        "Topics in Journalism: Introduction to Data Journalism","Wesleyan University","US","Undergraduate","Module","https://iasext.wesleyan.edu/regprod/!wesmaps_page.html?crse=014813&term=1171",
        "Data Journalism","Peking University","CN","Undergraduate","Module","http://elective.pku.edu.cn/elective2008/edu/pku/stu/elective/controller/courseDetail/getCourseDetail.do?kclx=BK&course_seq_no=BZ1617301834110_13521",
        "The Lede Program","Columbia University","US","Postgraduate","Programme","http://ledeprogram.com/",
        "Datenjournalismus","TU Dortmund","DE","Undergraduate","Programme","https://wj.ifj.tu-dortmund.de/datenjournalismus/studienschwerpunkt/",
        "Data Journalism - Laboratorio di strumenti e applicazioni per i media digitali","Sapienza Universita di Roma","IT","Undergraduate","Module","http://www.coris.uniroma1.it/corso/17500",
        "Laboratorio: Introduction to data journalism","Universita degli Studi di Torino","IT","Undergraduate","Module","http://www.didattica-est.unito.it/do/corsi.pl/Show?_id=94mg;sort=DEFAULT;search=;hits=106",
        "Data Journalism","Babe??-Bolyai University","RO","Undergraduate","Module","http://fspac.ubbcluj.ro/jurnalism/wp-content/uploads/2015/01/Data_journalism_en.pdf",
        "Data Journalism and Media Witnessing","University of Amsterdam","NL","Undergraduate","Module","https://www.uva.nl/shared-content/programmas/en/bachelors/media-and-information/study-programme/study-programme.html",
        "Political Data Journalism","University of Zurich","CH","Postgraduate","Programme","https://www.ipz.uzh.ch/en/studium/MA/tracks/dj.html",
        "Data Journalism POL-D.136","Maria Curie-Sk©©odowska University","PL","Undergraduate","Programme","https://usosweb.umcs.pl/kontroler.php?_action=katalog2/przedmioty/pokazPrzedmiot&prz_kod=POL-D.136",
        "Master en Periodismo de Datos","Centro Universitario Villanueva","ES","Postgraduate","Programme","http://www.universia.es/estudios/villanueva/master-periodismo-datos/st/240485",
        "¬®¬Ñ¬Ô¬Ú¬ã¬ä¬Ö¬â¬ã¬Ü¬Ñ¬ñ ¬á¬â¬à¬Ô¬â¬Ñ¬Þ¬Þ¬Ñ ¡ì¬¨¬å¬â¬ß¬Ñ¬Ý¬Ú¬ã¬ä¬Ú¬Ü¬Ñ ¬Õ¬Ñ¬ß¬ß¬í¬ç¡í","HSE","RU","Postgraduate","Programme","https://www.hse.ru/ma/datajourn/",
        "Atelier Datajournalisme","Unine","CH","Postgraduate","Module","-",
        "Datajournalism","HMKW","DE","Postgraduate","Module","-",
        "Data Journalism Research and Reporting","AMC Metropolitan College","GR","Undergraduate","Module","https://www.mitropolitiko.edu.gr/en/bachelor-degrees-undergraduate-programs/faculty-culture-communication/ba-in-media-production-journalism",
        "Data Journalism | EUJ 209","Aristotle University of Thessaloniki","GR","Postgraduate","Module","http://media.jour.auth.gr/data-journalism/",
        "Data and Media Communication Concentration (DMC)","Hong Kong Baptist University","HK","Undergraduate","Programme","http://bu-dmc.hkbu.edu.hk/",
        "Journalisme Audiovisuel, Supports Mobiles et Data-Journalisme","ISIC","MA","Postgraduate","Module","-",
        "Datajournalistik","University of Gothenburg","SE","Undergraduate","Module","-",
        "Data Journalism and Visualization","University of Gothenburg","SE","Postgraduate","Module","http://utbildning.gu.se/education/courses-and-programmes/program_detail/?programid=S2JOU",
        "Data Journalism","American University of Central Asia","KG","Undergraduate","Programme","https://auca.kg/en/auca_news/2962/",
        "Data Journalism","American University of Central Asia","KG","Postgraduate","Programme","https://dss.auca.kg/master-in-journalism-and-mass-communications/",
        "Data Literacy","University of Khartoum","SD","Undergraduate","Module","https://www.sudandata.org/learning/2",
        "Data Literacy","Ahfad University","SD","Undergraduate","Module","https://www.sudandata.org/learning/2",
        "Data Journalism","University of Tirana","AL","Undergraduate","Module","http://www.odecanet.org/data-journalism-manual/",
        "MA in Interactive Journalism","City University London","UK","Postgraduate","Programme","https://www.city.ac.uk/courses/postgraduate/interactive-journalism",
        "JOUR307 Data Journalism","University of Nebraska-Lincoln","US","Undergraduate","Module","https://bulletin.unl.edu/courses/JOUR/307",
        "J298 Data Journalism","University of California, Berkeley","US","Postgraduate","Module","https://journalism.berkeley.edu/course-section/j298-data-journalism/")

course <- matrix(data, nrow = 50, ncol = 6, byrow = T)

data.journalism <- data.frame(course)
names(data.journalism)<- c("Name","Institution","Country","Level","Type","URL")

table(data.journalism$Type)

data_graduate <- data.frame(data.journalism[data.journalism$Level == 'Postgraduate',])
data_graduate_prog <- data_graduate %>% 
  filter(Level == "Postgraduate" & Type == "Programme")

data_graduate_mod <- data_graduate %>%
  filter(Level == "Postgraduate" & Type == "Module")


