## Propagation of Error: Approving Citations to Problematic Research

Most claims that are made in a typical scientific paper are based on work done by other scientists. That is how science works. But when scientists approvingly cite---cite without noting any concern---problematic research, this bedrock of science turns to mush. In this paper, we investigate how common approving citations to problematic research are, and how much does the rate of citation vary before and after the problems are publicized. 

Given some of problems likely stem from the fact that it is still not easy to find problematic research, we have developed a [web application](http://re-cite.org) that allows people to quickly check whether they cite retracted research.

-----------

### Table of Contents:

* [Data](#data)
* [Scripts](#scripts)
* [Manuscript](#manuscript)

-------

### Data

* **Nieuwenhuis et al.**
    
    * [Original Data by Nieuwenhuis](data/01_nieuwenhuis/from_nieuwenhuis/)
    * [Citations to Articles in Nieuwenhuis Data (see the excel files)](data/01_nieuwenhuis/)
    * [Are citations to articles with errors approving?](data/02_are_nw_citations_approving/): We coded a random sample of articles citing articles with errors after the publication of Nieuwenhuis et al. to check if the citations were *approving*.

* **Web of Science (WoS)**

    * [What is in the WoS Corpus?](data/11_wos/what_is_in_wos/): List of some of the journals and conferences included in the Web of Science Corpus by subject area.
    * [Number of records by research area by year in WoS](data/11_wos/wos_pubs_per_year/)

* **WoS Codebook**

    * [Codebook for WoS citation data](data/citation_codebook.csv)

* **WoS Research Area/Categories**

    * [List of WoS Research Categories](data/11_wos/wos_wc_list.csv)
    * [List of Research Areas](data/11_wos/wos_sc_list.csv)
    * [Crosswalk between the two](data/11_wos/wos_journal_resarea_subcat_crosswalk.csv)
    
* **Journal Impact Factor**

    * [Journal Impact Factor](data/journal_impact_factor.csv)

* **Retraction Notices:**

    * [Retraction Notices](data/03_retraction_notices/): All the WoS fields for all the retraction notices plus `index` field that carries unique identifier for each of the notices.

    * [Why were articles retracted?](data/04_reason_for_retraction/): We coded a random sample of retraction notices to understand the reasons for retraction.

* **Retracted Articles**

    * [Retracted Articles](data/05_retracted_articles/): All the WoS fields for all the articles that are eventually retracted plus an `index` field that carries the `index` of the relevant retraction notice.

    * [Are Retracted Articles Actually Retracted?](data/06_are_retracted_articles_retracted/): We coded a random sample to check the integrity of our data.
 
* **Citations to Retracted Articles**

    * [Citations to Retracted Articles](data/07_citations_to_retracted_articles/): All the WoS fields for all the citations to retraction notices along with an `index` field that carries the index of the relevant retraction notice/retracted article.

    * [Are Citations to Retracted Articles Approving?](data/09_are_post_retration_citations_approving/): We coded a random sample of citations before retraction, and a random sample of citations after retraction to check if citations are *approving*.

* **Citations to Retraction Notices**

    * [Citations to Retraction Notices](data/08_citations_to_retraction_notices/): All the WoS fields for all the citations to retracted articles along with an `index` field that carries the index of the relevant retraction notice.

* **Check for Dates**
    
    * [Check Dates](data/10_bad_dates/): Given articles are increasingly first published online before print, there are some citations that may refer to online than the print version. WoS catalogs the print data. We wanted to check if this issue would make us code some articles published before retraction as post-publication or vice-versa.

### Scripts 

* [Nieuwenhuis](scripts/01_nieuwenhuis.R): All the analysis related to Nieuwenhuis.

* [Describe retraction notices](scripts/02_describe_retraction_notices.R): What are the reasons for retraction, etc.?

* [Time to retraction](scripts/03_time_to_retraction.R): Time to retraction

* [How many citations to retracted articles pre- and post- retraction  are approving?](scripts/04_are_pre_post_retraction_citations_approving.R)

* [Citations to retracted articles](scripts/05_citations_to_retracted_articles.R)

### Manuscript

* [manuscript (.tex, .pdf, .bib)](ms/)

### Authors

Ken Cor and Gaurav Sood

### Contribute to the project

If you 'see something', create a pull request or issue for that something! Be it an inconsistency in the data, issue with the analysis or with the writing, a suggestion, data you would like to contribute, or something else entirely. 

