package library

object AnalyserHtml extends FiltrageHtml{
    /**
   * A partir d'un document Html h et d'une requête e, dit si le document
   * satisfait l'expression e
   *
   * @param h le document Html
   * @param e l'expression
   * @return true si le document satisfait l'expression e
   */
  def filtreHtml(h: Html, e: Expression): Boolean = {
    val str = html2String(h)
    contient(e, str)
  }
  
  /**
   * A partir d'une list de Html html, nous recuperons une liste de mots
   *
   * @param h la liste de Html
   * @return une chaine de caractères contenant liste de mots.
   */
   def html2String(html :Html):String = {
     html match{
       case Text(mot) => mot
       case Tag(_,_,list) => html2String(list)
     }
   }
   
   def html2String(html: List[Html]): String = {
     html match {
       case Nil => ""
       case h :: end => html2String(h) + html2String(end)
     }
   }
  
    
  /**
   * A partir d'une expression e et d'une chaine de caractère s, informe si l'expression e
   * contient la chaine s
   *
   * @param e l'expression
   * @param e l'expression
   * @return true si la chaine de caractères est inclut dans l'expression e
   */
   def contient(e:Expression, s:String): Boolean = {
    e match{
      case And(e1, e2) => contient(e1, s) && contient(e2, s)
      case Or(e1, e2) => contient(e1, s) || contient(e2, s)
      case Word(mot) => s.toLowerCase().contains(mot.toLowerCase())  
      }
    }
   
}