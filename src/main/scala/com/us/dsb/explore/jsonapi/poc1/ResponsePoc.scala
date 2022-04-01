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

  import EntityMetadata._
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
      //println(s"makeData.x1: dbRowOpt = " + dbRowOpt)

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
        "self" -> Json.fromString(s"$apiUrlPathPrefix/${getEntityTypeSegment(`type`).raw}/${entityId.raw}")
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
      //println(s"makeData.x1: dbRows = " + dbRows)

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