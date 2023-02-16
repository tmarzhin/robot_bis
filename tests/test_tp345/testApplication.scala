package test_tp345

import org.junit.Test

import org.junit.Assert._
import library.ExpressionParser
import library.Expression
import library.MonApp._

class testApplication {
  
  /**
   * test getKeywords
   */
  @Test
  def testGetKeywords() {
    val expressions: List[Expression] = List(
      ExpressionParser.LocalParser.parse("a and b and c or d or e and f and g").get,
      ExpressionParser.LocalParser.parse("a and b and c and d").get,
      ExpressionParser.LocalParser.parse("a and b and c or d").get,
      ExpressionParser.LocalParser.parse("a or b and c and d").get,
      ExpressionParser.LocalParser.parse("a or b or c or d").get,
      ExpressionParser.LocalParser.parse("a").get,
      ExpressionParser.LocalParser.parse("a or b").get,
      ExpressionParser.LocalParser.parse("a and b").get)
      
    val keywords: List[List[String]] = List(
        List("a+b", "c", "d", "e", "f+g"),
        List("a+b+c+d"),
        List("a+b", "c", "d"),
        List("a", "b", "c+d"),
        List("a", "b", "c", "d"),
        List("a"),
        List("a", "b"),
        List("a+b"))
      
    for ((e, k) <- expressions zip keywords) {
      assertEquals(getKeywords(e), k)
    }
    
  }
  
  /**
   * test getLinksFromKeywords
   */
  @Test
  def testGetLinksFromKeywords() {
    val debutLien: String = "https://search.vivastreet.com/annonces/fr"
    val milieuLien: String = "?lb=new&search=1&start_field=1&keywords="
    val finLien: String = "&cat_1=&geosearch_text=&searchGeoId=0"
    val keyword: String = "voiture+rennes"
    val expectedResult: List[String] = List(
      debutLien + "" + milieuLien + keyword + finLien,
      debutLien + "/t1+" + milieuLien + keyword + finLien,
      debutLien + "/t+2" + milieuLien + keyword + finLien,
      debutLien + "/t+3" + milieuLien + keyword + finLien,
      debutLien + "/t+4" + milieuLien + keyword + finLien,
      debutLien + "/t+5" + milieuLien + keyword + finLien,
      debutLien + "/t+6" + milieuLien + keyword + finLien,
      debutLien + "/t+7" + milieuLien + keyword + finLien,
      debutLien + "/t+8" + milieuLien + keyword + finLien,
      debutLien + "/t+9" + milieuLien + keyword + finLien,
      
    )
    
    val result: List[String] = getLinksFromKeywords(List(milieuLien + keyword + finLien), 10)
    
    for ((r, e) <- result zip expectedResult) {
      assertEquals(e, e)
    }
  }
  
  /**
   * test getLinksFromKeywords
   */
  @Test
  def testGetLinksFromKeywords2() {
    val debutLien: String = "https://search.vivastreet.com/annonces/fr"
    val milieuLien: String = "?lb=new&search=1&start_field=1&keywords="
    val finLien: String = "&cat_1=&geosearch_text=&searchGeoId=0"
    val keyword1: String = "voiture+rennes"
    val keyword2: String = "dev+nantes"
    val expectedResult: List[String] = List(
      debutLien + "" + milieuLien + keyword1 + finLien,
      debutLien + "/t1+" + milieuLien + keyword1 + finLien,
      debutLien + "/t+2" + milieuLien + keyword1 + finLien,
      debutLien + "" + milieuLien + keyword2 + finLien,
      debutLien + "/t1+" + milieuLien + keyword2 + finLien,
      debutLien + "/t+2" + milieuLien + keyword2 + finLien,
      
    )
    
    val result: List[String] = getLinksFromKeywords(List(milieuLien + keyword1 + finLien, milieuLien + keyword2 + finLien), 2)
    
    for ((r, e) <- result zip expectedResult) {
      assertEquals(e, e)
    }
  }
  
  /**
   * test GetLinksFromKeyword
   */
  @Test
  def testGetLinksFromKeyword() {
    val debutLien: String = "https://search.vivastreet.com/annonces/fr"
    val finLien: String = "/fin"
    
    val expectedResult: List[String] = List(
      debutLien + "" + finLien,
      debutLien + "/t1+" + finLien,
      debutLien + "/t+2" + finLien,
      debutLien + "/t+3" + finLien,
      debutLien + "/t+4" + finLien,
      debutLien + "/t+5" + finLien,
      debutLien + "/t+6" + finLien,
      debutLien + "/t+7" + finLien,
      debutLien + "/t+8" + finLien,
      debutLien + "/t+9" + finLien,
      debutLien + "/t+10" + finLien
      
    )
    val result: List[String] = getLinksFromKeyword(finLien, List(), 10)
    
    for ((r, e) <- result zip expectedResult) {
      assertEquals(e, e)
    }
  }
}