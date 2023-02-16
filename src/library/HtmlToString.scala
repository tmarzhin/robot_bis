package library

object HtmlToString extends Html2String {
  
  def process(h: Html): String = {
    "<!DOCTYPE html>" + getTag(h)
  }
  
  private def getTag(h:Html):String = {
    h match {
      case Tag(name, attributes, children) => "\n<" + name + getAttributes(attributes) + ">" +
                                              getTags(children) + 
                                              "</" + name+">"
      case Text(text) => text
    }
  }
  
  /** Produit la chaîne de caractère correspondant aux enfants d'un élément HTML 
   *  
   *  @param l la liste d'enfants d'un élément Html
   *  @return la chaîne de caractère représentant l
   */
  private def getTags(l:List[Html]):String={
    l match {
      case Nil => ""
      case htmlElement :: restOfList => getTag(htmlElement) + getTags(restOfList)
    }
  }
  
  /** Produit la chaîne de caractère correspondant aux attributs d'un HTML élément
   *  
   *  @param l la liste de couples d'attributs d'un élément Html
   *  @return la chaîne de caractère représentant l
   */
  private def getAttributes(attributes:List[(String,String)]):String = {
    attributes match {
      case Nil => ""
      case att :: nextAtt => att match {
                             case (name, value) => " " + name + "=" +
                                                   "\"" + value + "\"" + 
                                                   getAttributes(nextAtt)
                             } 
    }
  }

  
}