package com.us.dsb.explore.jsonapi.poc1

import com.us.dsb.explore.jsonapi.poc1.Database._
import io.circe.Json

import java.net.URI

object ResponsePoc extends App {
  /*
   - scalar data: "data" will be for one entity, not list
   - "data": type, id, attributes w/some, relationships w/some
   - "meta": entity type name, attributes and types, relationships and maybe types
   - "links": "self", no "related"
   - "included": not initially

   */

  type TBD = Unit



  object EntityMetadata {
    trait DataType

    case object DT_String extends DataType
    case object DT_Int extends DataType
    case object DT_Timestamp extends DataType
    case class DT_Enumeration(enumerators: TBD) extends DataType //?? where do specific enumeration types go?
    case class DT_JsonObject1() extends DataType // (maybe)
    case class DT_JsonObjec2t(Schema: TBD) extends DataType // (possibly)

    case class DataTypeString(raw: String) extends AnyVal

    sealed trait EntityType
    sealed trait Attribute

    case class EntityTypeName(raw: String) extends AnyVal
    case class EntityTypeSegment(raw: String) extends AnyVal
    case class EntityId(raw: String) extends AnyVal
    case class AttributeName(raw: String) extends AnyVal

  }

  import EntityMetadata._

  trait EntityMetadata {
    /** (Currently,) not necessarily simple name--e.g., with enumerators */
    def getDataTypeString(`type`: DataType): DataTypeString
    def getEntityTypeName(`type`: EntityType): EntityTypeName
    def getEntityTypeSegment(`type`: EntityType): EntityTypeSegment
    def getEntityTypeAttributes(`type`: EntityType): Seq[Attribute]
    def getEntityTableName(`type`: EntityType): TableName
    def getEntityTableKeyColumn(`type`: EntityType): ColumnName
    def getAttributeName(attribute: Attribute): AttributeName
    def getAttributeType(attribute: Attribute): DataType
    def getAttributeColumnName(attribute: Attribute): ColumnName
  }

  object EntityMetadataImpl extends EntityMetadata {
    override def getDataTypeString(`type`: DataType): DataTypeString = {
      val nameThis =
        `type` match {
          case DT_String => "string"
          case DT_Int => "int"
        }
      DataTypeString(nameThis)
    }

    case object UserType extends EntityType

    case object User_ObjectGuid extends Attribute
    case object User_UserName extends Attribute
    case object User_SomeInt extends Attribute

    override def getEntityTypeName(`type`: EntityType): EntityTypeName = {
      `type` match {
        case UserType => EntityTypeName("user")
      }
    }

    override def getEntityTypeSegment(`type`: EntityType): EntityTypeSegment = {
      `type` match {
        case UserType => EntityTypeSegment("users")
      }
    }

    override def getEntityTableName(`type`: EntityType): TableName = {
      `type` match {
        case UserType => DatabaseImpl.TableNames.users
      }
    }

    override def getEntityTableKeyColumn(`type`: EntityType): ColumnName = {
      `type` match {
        case UserType => DatabaseImpl.UserColumnNames.object_guid
      }
    }

    override def getEntityTypeAttributes(`type`: EntityType): Seq[Attribute] = {
      `type` match {
        case UserType => List(User_ObjectGuid, User_UserName, User_SomeInt)
        case _ => ???
      }
    }

    override def getAttributeName(attribute: Attribute): AttributeName = {
      val nameThis =
        attribute match {
          case User_ObjectGuid => "objectGuid"
          case User_UserName => "userName"
          case User_SomeInt => "someInt"
        }
      AttributeName(nameThis)
    }

    override def getAttributeType(attribute: Attribute): DataType = {
      attribute match {
        case User_ObjectGuid => DT_String
        case User_UserName => DT_String
        case User_SomeInt => DT_Int
        //?? do something with enumeration
        //?? maybe do some times with same enumeration; how to share?
        // (should "meta" have a "datatypes" member for ~parameterized data-type
        //    classes (e.g., enumeration classes)? should all types be whether,
        //    with simple ones such as "string" being declared as primitive or
        //    build it?)
      }
    }

    override def getAttributeColumnName(attribute: Attribute): ColumnName = {
      attribute match {
        case User_ObjectGuid => DatabaseImpl.UserColumnNames.object_guid
        case User_UserName => DatabaseImpl.UserColumnNames.user_name
        case User_SomeInt => DatabaseImpl.UserColumnNames.some_int
      }
    }

  }
  import EntityMetadataImpl._

  def makeTopMetadata(`type`: EntityType): Json = {
    val typeName = getEntityTypeName(`type`)
    val allAttributes = getEntityTypeAttributes(`type`)
    val attributesValue: Json = {
      Json.fromValues(
        allAttributes.map { attribute =>
          val typeStr = getDataTypeString(getAttributeType(attribute))
          Json.obj(
            "name" -> Json.fromString(getAttributeName(attribute).raw),
            "type" -> Json.fromString(typeStr.raw)
            //?? expand "type" to object--allow for more type info (phys., log.; class, parameterized)
            )
        }
        )
    }
    val entityTypeValue =
      Json.obj(
        "typeName" -> Json.fromString(typeName.raw),
        // typeUrlPathSegment
        "attributes" -> attributesValue
        //"relationships"

        )

    Json.obj(
      "entityTypes" -> Json.obj(typeName.raw -> entityTypeValue)
      //?? dataTypes if we need to declare names for enumeration types (having
      //  enumerators list separate from references to enumeration type)
      //?? non-type metadata, e.g., entity counts

      )
  }


  def renderRow(apiUrlPathPrefix: URI,
                `type`: EntityType,
                requestedAttributes: Seq[Attribute],
                rowColumnNameToValueMap2: Map[ColumnName, Any]): Json = {

    def dbAnyToJson(any: Any): Json = {
      any match {
        case s: String => Json.fromString(s)
        case i: Int => Json.fromInt(i)
      }
    }

    val attributesObject = {
      val fields: Iterable[(String, Json)] =
        requestedAttributes.map { attr =>
          val dbColumn = getAttributeColumnName(attr)
          val value = rowColumnNameToValueMap2(dbColumn)
          val attrName = EntityMetadataImpl.getAttributeName(attr)
          attrName.raw -> dbAnyToJson(value)
        }
      Json.fromFields(fields)
    }
    val entityId = {
      val dbColumn = getEntityTableKeyColumn(`type`)
      val value = rowColumnNameToValueMap2(dbColumn)
      value
    }
    val typeName = getEntityTypeName(`type`)
    val rowResourceObject =
      Json.obj(
        "type" -> Json.fromString(typeName.raw),
        "id" -> dbAnyToJson(entityId),
        "attributes" -> attributesObject,
        // (no "relationships" yet or in this case)
        "links" -> Json.obj(
          "self" -> Json.fromString(s"$apiUrlPathPrefix/${getEntityTypeSegment(`type`).raw}/$entityId")

          )
        )
    rowResourceObject
    //????? :factor out row rendering
  }

  def makeSingleEntityResponse(apiUrlPathPrefix: URI, // (concat., don't resolve)
                               `type`          : EntityType,
                               entityId        : EntityId,
                               other           : TBD): Json = {
    val requestedAttributes = getEntityTypeAttributes(`type`)  //??? factor out/move up

    def makeData: Json = {

      //??? factor out commonality (from single- vs. multiple-entity methods)
      //?? add entity ID column (primary key) if not explicitly requested
      val requestedDbColumns =
      requestedAttributes.map { attribute =>
        getAttributeColumnName(attribute)
      }

      val table = getEntityTableName(`type`)

      // - execute query (into some intermediate data)
      val dbRowOpt = DatabaseImpl.selectSpecificRow(table, RowKey(entityId.raw), requestedDbColumns: _*)
      println(s"makeData.x1: dbRowOpt = " + dbRowOpt)

      dbRowOpt match {
        case None => ???  //??? untangle no-such-entity (404) case
        case Some(rowColumnNameToValueMap1) =>


        val xxresourceObject =
          renderRow(apiUrlPathPrefix, `type`, requestedAttributes, rowColumnNameToValueMap1)

        xxresourceObject
      }

    }

    // top-level object
    Json.obj(
      "links" -> Json.obj(
        //?? later, include relevant query parameters
        "self" -> Json.fromString(s"$apiUrlPathPrefix/${getEntityTypeSegment(`type`).raw}")
        //?? links: pagination
        ),
      "meta" -> makeTopMetadata(`type`),
      "data" -> makeData
      )
  }

  def makeEntityCollectionResponse(apiUrlPathPrefix: URI, // (concat., don't resolve)
                                   `type`          : EntityType,
                                   other           : TBD): Json = {
    val requestedAttributes = getEntityTypeAttributes(`type`)  //??? factor out/move up

    def makeData: Json = {

      //??? factor out commonality (from single- vs. multiple-entity methods)
      //?? add entity ID column (primary key) if not explicitly requested
      val requestedDbColumns =
      requestedAttributes.map { attribute =>
        getAttributeColumnName(attribute)
      }

      val table = getEntityTableName(`type`)

      // - execute query (into some intermediate data)
      val dbRows = DatabaseImpl.selectAllRows(table, requestedDbColumns: _*)
      println(s"makeData.x1: dbRows = " + dbRows)

      // - construct JSON response entity data
      val resourceObjects = dbRows.map { rowColumnNameToValueMap1 =>
        renderRow(apiUrlPathPrefix, `type`, requestedAttributes, rowColumnNameToValueMap1)
      }
      Json.fromValues(resourceObjects)
    }

    // top-level object
    Json.obj(
      "links" -> Json.obj(
        //?? later, include relevant query parameters
        "self" -> Json.fromString(s"$apiUrlPathPrefix/${getEntityTypeSegment(`type`).raw}")
        //?? links: pagination
        ),
      "meta" -> makeTopMetadata(`type`),
      "data" -> makeData
      )
  }


  {
    val responseDoc =
      makeSingleEntityResponse(URI.create("/someApi"),
                                   UserType,
                                   EntityId("user0123-fake-guid"),
                                   ())
    println(s"ResponsePoc.makeSingleEntityResponse: responseDoc = $responseDoc")
  }
  {
    val responseDoc =
      makeEntityCollectionResponse(URI.create("/someApi"),
                                   UserType,
                                   ())
    println(s"ResponsePoc.makeEntityCollectionResponse: responseDoc = $responseDoc")
  }
}
