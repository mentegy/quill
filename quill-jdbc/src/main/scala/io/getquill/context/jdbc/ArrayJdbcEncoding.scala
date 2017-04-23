package io.getquill.context.jdbc

import java.sql.{ JDBCType, Types }

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait ArrayJdbcEncoding {
  self: JdbcContext[_, _] =>

  implicit def seqStringEncoder[Col <: Traversable[String]: ClassTag]: Encoder[Col] = rawEncoder[String, Col](Types.VARCHAR)

  implicit def seqStringDecoder[Col <: Traversable[String]](implicit bf: CanBuildFrom[Nothing, String, Col]): Decoder[Col] = rawDecode[String, Col](Types.VARCHAR)

  private def rawEncoder[T, Col <: Traversable[T]: ClassTag](jdbcType: Int): Encoder[Col] = seqEncoder[T, Col](jdbcType, _.asInstanceOf[AnyRef])
  private def rawDecode[T, Col <: Traversable[T]](jdbcType: Int)(implicit bf: CanBuildFrom[Nothing, T, Col]): Decoder[Col] = seqDecoder[T, T, Col](jdbcType, identity)

  protected def seqEncoder[T, Col <: Traversable[T]: ClassTag](jdbcType: Int, mapper: T => AnyRef): Encoder[Col] = {
    encoder[Col](Types.ARRAY, (idx: Index, seq: Col, row: PrepareRow) => {
      row.setArray(
        idx,
        withConnection(_.createArrayOf(
          parseJdbcType(jdbcType),
          seq.map(x => mapper(x)).toArray
        ))
      )
    })
  }

  protected def seqDecoder[I, O, Col <: Traversable[O]](jdbcType: Int, mapper: I => O)(implicit bf: CanBuildFrom[Nothing, O, Col]): Decoder[Col] = {
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

  protected def parseJdbcType(intType: Int): String = JDBCType.valueOf(intType).getName
}
