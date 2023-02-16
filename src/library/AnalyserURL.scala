package library



object AnalyserURL extends FiltrageURLs {
  
  val tools = UrlProcessor
  
  /** A partir d'un document Html h, rend la liste des URLs accessibles à partir
    de h (ces URLs sont des hyperliens h) tels que ces URLs sont tous des URLs
    d'annonces du site de référence
 
    @param h le document Html
    @return la liste des URLs d'annonces contenues dans h
 */
  def filtreAnnonce(h:Html):List[String]={
    h match {
      case Text(_) => List()
      case Tag("a",urls,_) => val lien = getHref(urls); if(tools.host(lien) == "https://www.vivastreet.com/") List(lien) else List()
      case Tag(_,_,hn) => filtreAux(hn)
    }
  }
  
  def getHref(attr: List[(String,String)]): String = {
    attr match {
      case Nil => ""
      case (n,v) :: end => if (n == "href") v else getHref(end)
    }
  }
  
  private def filtreAux(h:List[Html]):List[String]={
    h match {
      case Nil => Nil
      case a::r => filtreAnnonce(a)++filtreAux(r)
    }
  }

  
}

