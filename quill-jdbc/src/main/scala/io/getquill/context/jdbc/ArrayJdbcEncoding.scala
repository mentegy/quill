package io.getquill.context.jdbc

import java.sql.{ JDBCType, Types }
import io.getquill.dsl.TraversableEncoding
import scala.collection.generic.CanBuildFrom

trait ArrayJdbcEncoding extends TraversableEncoding {
  self: JdbcContext[_, _] =>

  implicit def arrayStringEncoder[Col <: Traversable[String]]: Encoder[Col] = rawEncoder[String, Col](Types.VARCHAR)
  implicit def arrayStringDecoder[Col <: Traversable[String]](implicit bf: CanBuildFrom[Nothing, String, Col]): Decoder[Col] = rawDecoder[String, Col](Types.VARCHAR)

  private def rawEncoder[T, Col <: Traversable[T]](jdbcType: Int): Encoder[Col] =
    arrayEncoder[T, Col](jdbcType, _.asInstanceOf[AnyRef])

  private def rawDecoder[T, Col <: Traversable[T]](jdbcType: Int)(implicit bf: CanBuildFrom[Nothing, T, Col]): Decoder[Col] =
    arrayDecoder[T, T, Col](jdbcType, identity)

  protected def parseJdbcType(intType: Int): String = JDBCType.valueOf(intType).getName

  def arrayEncoder[T, Col <: Traversable[T]](jdbcType: Int, mapper: T => AnyRef): Encoder[Col] = {
    encoder[Col](Types.ARRAY, (idx: Index, seq: Col, row: PrepareRow) => {
      val bf = implicitly[CanBuildFrom[Nothing, AnyRef, Array[AnyRef]]]
      row.setArray(
        idx,
        withConnection(_.createArrayOf(
          parseJdbcType(jdbcType),
          seq.foldLeft(bf())((b, x) => b += mapper(x)).result()
        ))
      )
    })
  }

  def arrayDecoder[I, O, Col <: Traversable[O]](jdbcType: Int, mapper: I => O)(implicit bf: CanBuildFrom[Nothing, O, Col]): Decoder[Col] = {
    decoder[Col](Types.ARRAY, (idx: Index, row: ResultRow) => {
      val arr = row.getArray(idx)
      if (arr == null) bf().result()
      else {
        val resType = arr.getBaseType
        require(
          resType == jdbcType || parseJdbcType(arr.getBaseType) == parseJdbcType(resType),
          s"Retrieved unsupported jdbc type: ${parseJdbcType(jdbcType)}, expected: ${arr.getBaseTypeName}"
        )
        arr.getArray.asInstanceOf[Array[I]]
          .foldLeft(bf())((b, x) => b += mapper(x))
          .result()
      }
    })
  }
}
