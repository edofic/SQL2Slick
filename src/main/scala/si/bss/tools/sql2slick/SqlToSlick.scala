package si.bss.tools.sql2slick

/**
 * User: bss
 * Date: 6/19/13
 * Time: 12:20 AM
 */

import StringCamelImplicits._

object SqlToSlick {

  def apply(sql: String): Either[String,String] = {
    SqlFieldsParser.parseAll(SqlFieldsParser.table, sql) map {
      case (tableName, fields) =>
        val className = tableName.toCapitalizedCamelCaseIdent
        val caseClass = SlickGenerator.genCaseClass(className, fields)
        val mapped = SlickGenerator.genMappedTable(tableName,className,fields)
        caseClass+"/n/n/n"+mapped
    } match {
      case SqlFieldsParser.Success(txt: String, _) => Right(txt)
      case SqlFieldsParser.NoSuccess(txt, input) => Left(s"$txt; $input")
    }
  }

  def main(args: Array[String]) {

    println("--------------------------------------------------------------")
    println(" Please paste your SQL table definition (followed by CTRL+D):")
    println("--------------------------------------------------------------")

    val input = io.Source.stdin.getLines().mkString("\n")
    val parsed = apply(input)

    println("--------------------------------------------------------------")

    val output = parsed match {
      case Right(code) =>
        s"""
          |--------------------------------------------------------------
          | Parsed successfully:
          | Please copy the code below:
          |--------------------------------------------------------------
          |
          |$code
        """.stripMargin

      case Left(error) =>
        s"""
          |--------------------------------------------------------------
          | Parsing failed.
          |--------------------------------------------------------------
          |
          |$error
        """.stripMargin

    }
    println(output)
  }
}
