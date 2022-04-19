package com.us.dsb.explore.jsonapi.poc1

import com.us.dsb.explore.jsonapi.poc1.Database._
import io.circe.Json

import java.net.URI


object EntityMetadata {
  type TBD = Unit

  trait DataTypeKind
  case class DataKindName(raw: String) extends AnyVal

  //(??: "primitive"? "predefined"? other?
  case object PrimitiveKind   extends DataTypeKind
  case object EnumerationKind extends DataTypeKind
  //?? maybe some JsonKind for JSON object types (with schema(?))

  trait DataType
  case class DataTypeName(raw: String) extends AnyVal

  case object DT_String extends DataType
  case object DT_Int extends DataType
  case object DT_Timestamp extends DataType

  case class EnumeratorName(raw: String) extends AnyVal
  case class DT_Enumeration(typeName: DataTypeName,
                            enumerators: EnumeratorName*) extends DataType

  case class DT_JsonObject1()            extends DataType  // (maybe)
  case class DT_JsonObject2(Schema: TBD) extends DataType  // (possibly)


  //??? address physical type vs. logical type(s)

  sealed trait EntityType
  sealed trait Attribute
  //?? relationships (outgoing half?)

  //?? (refine to member-syntax strings, etc.:)

  case class EntityTypeName(raw: String) extends AnyVal
  case class EntityTypeSingularLabel(raw: String) extends AnyVal
  case class EntityTypePluralLabel(raw: String) extends AnyVal
  case class EntityTypeSegment(raw: String) extends AnyVal

  case class EntityId(raw: String) extends AnyVal

  case class AttributeName(raw: String) extends AnyVal
  case class AttributeLabel(raw: String) extends AnyVal  //(exposed name)

}

import EntityMetadata._

trait EntityMetadata {
  def getDataKindName(kind: DataTypeKind): DataKindName

  def getDataTypeName(`type`: DataType): DataTypeName
  def getDataTypeKind(`type`: DataType): DataTypeKind

  def getEntityTypeName(         `type`: EntityType): EntityTypeName
  def getEntityTypeSingularLabel(`type`: EntityType): EntityTypeSingularLabel
  def getEntityTypePluralLabel(  `type`: EntityType): EntityTypePluralLabel
  def getEntityTypeSegment(      `type`: EntityType): EntityTypeSegment
  def getEntityTypeAttributes(   `type`: EntityType): Seq[Attribute]
  def getEntityTableName(        `type`: EntityType): TableName
  def getEntityTableKeyColumn(   `type`: EntityType): ColumnName

  def getEntityTypeForSegment(segment: EntityTypeSegment): EntityType

  def getAttributeName(attribute: Attribute): AttributeName
  def getAttributeLabel(attribute: Attribute): AttributeLabel
  def getAttributeType(attribute: Attribute): DataType
  def getAttributeColumnName(attribute: Attribute): ColumnName
}

object EntityMetadataImpl extends EntityMetadata {

  override def getDataKindName(kind: DataTypeKind): DataKindName = {
    kind match {
      case PrimitiveKind   => DataKindName("primitive")
      case EnumerationKind => DataKindName("enumeration")
      case _ =>
        println(s"getDataTypeString: unknown data-type kind '$kind'")
        ???
    }
  }

  override def getDataTypeName(`type`: DataType): DataTypeName = {
    `type` match {
      case DT_String                => DataTypeName("string")
      case DT_Int                   => DataTypeName("int")
      case DT_Enumeration(name, _*) => name
      case _ =>
        println(s"getDataTypeString: unknown data type '${`type`}'")
        ???
    }
  }

  override def getDataTypeKind(`type`: DataType): DataTypeKind = {
    `type` match {
      case DT_String         => PrimitiveKind
      case DT_Int            => PrimitiveKind
      case _: DT_Enumeration => EnumerationKind
      case _ =>
        println(s"getDataTypeKind: unknown datatype '${`type`}'")
        ???
    }
  }

  //???? soon maybe getDataTypeKind (int, string, enum, "schemaed" JSON)

  // <??? what kind of> data types:
  //???? revisit:  consider:  case object with DT_Enumeration being non-case class?
  val DT_SomeEnum = DT_Enumeration(DataTypeName("someEnum"),
                                   EnumeratorName("One"),
                                   EnumeratorName("Two"))
  val DT_DomainEnum = DT_Enumeration(DataTypeName("domainEnum"),
                                     EnumeratorName("dough"),
                                     EnumeratorName("mein"))

  // Entity-type identifiers:
  case object UserType   extends EntityType
  case object DomainType extends EntityType

  // Entity-type-attribute identifiers:
  case object User_ObjectGuid   extends Attribute
  case object User_UserName     extends Attribute
  case object User_DomainName   extends Attribute
  case object User_SomeInt      extends Attribute
  case object User_SomeEnum     extends Attribute
  case object Domain_ObjectGuid extends Attribute
  case object Domain_DomainName extends Attribute
  case object Domain_DomainEnum extends Attribute

  // Entity-type--level data:

  private case class EntityTypeData(name: EntityTypeName,
                                    singularLabel: EntityTypeSingularLabel,
                                    pluralLabel: EntityTypePluralLabel,
                                    segment: EntityTypeSegment,
                                    tableName: TableName,
                                    tableKeyColumn: ColumnName,
                                    attributes: Seq[Attribute])

  private def getEntityTypeData(`type`: EntityType): EntityTypeData = {
    //?? rework into map/etc.
    import DatabaseImpl._
    `type` match {
      case UserType =>
        EntityTypeData(EntityTypeName("user"),
                       EntityTypeSingularLabel("User"),
                       EntityTypePluralLabel("Users"),
                       EntityTypeSegment("users"),
                       TableNames.users,
                       UserColumnNames.object_guid,
                       List(User_ObjectGuid,
                            User_UserName,
                            User_DomainName,
                            User_SomeInt,
                            User_SomeEnum))
      case DomainType =>
        EntityTypeData(EntityTypeName("domain"),
                       EntityTypeSingularLabel("Domain"),
                       EntityTypePluralLabel("Domains"),
                       EntityTypeSegment("domains"),
                       TableNames.domains,
                       DomainColumnNames.object_guid,
                       List(Domain_ObjectGuid,
                            Domain_DomainName,
                            Domain_DomainEnum))
    }
  }

  override def getEntityTypeName(`type`: EntityType): EntityTypeName =
    getEntityTypeData(`type`).name

  override def getEntityTypeSingularLabel(  `type`: EntityType): EntityTypeSingularLabel =
    getEntityTypeData(`type`).singularLabel
  override def getEntityTypePluralLabel(  `type`: EntityType): EntityTypePluralLabel =
    getEntityTypeData(`type`).pluralLabel

  override def getEntityTypeSegment(`type`: EntityType): EntityTypeSegment =
    getEntityTypeData(`type`).segment

  override def getEntityTableName(`type`: EntityType): TableName =
    getEntityTypeData(`type`).tableName

  override def getEntityTableKeyColumn(`type`: EntityType): ColumnName =
    getEntityTypeData(`type`).tableKeyColumn

  override def getEntityTypeAttributes(`type`: EntityType): Seq[Attribute] =
    getEntityTypeData(`type`).attributes


  def getEntityTypeForSegment(segment: EntityTypeSegment): EntityType = {
    segment match {
      case EntityTypeSegment("users") => UserType
    }
  }

  // Attribute--level data:

  private case class AttrData(name: AttributeName,
                              label: AttributeLabel,
                              `type`: DataType,
                              dbColumn: ColumnName)
  private def getAttributeData(attribute: Attribute): AttrData = {
    //?? rework into map/etc.
    import DatabaseImpl._

    val (name: String, label: String, `type`: DataType, colName: ColumnName) =
      attribute match {
        case User_ObjectGuid =>
          ("objectGuid", "GUID",        DT_String,   UserColumnNames.object_guid)
        case User_UserName =>
          ("userName",   "Name",        DT_String,   UserColumnNames.user_name)
        case User_DomainName =>
          ("domainName", "Domain Name", DT_String,   UserColumnNames.domain_name)
        case User_SomeInt =>
          ("someInt",    "Some Int",    DT_Int,      UserColumnNames.some_int)
        case User_SomeEnum =>
          ("someEnum",   "Some Enum",   DT_SomeEnum, UserColumnNames.some_enum)

        case Domain_ObjectGuid =>
          ("objectGuid", "GUID",        DT_String,     DomainColumnNames.object_guid)
        case Domain_DomainName =>
          ("domainName", "Name",        DT_String,     DomainColumnNames.domain_name)
        case Domain_DomainEnum =>
          ("domainEnum", "Domain Enum", DT_DomainEnum, DomainColumnNames.domain_enum)

        //?? do something with enumeration
        //?? maybe do some times with same enumeration; how to share?
        // (should "meta" have a "dataTypes" member for ~parameterized data-type
        //    classes (e.g., enumeration classes)? should all types be whether,
        //    with simple ones such as "string" being declared as primitive or
        //    build it?)
      }
    AttrData(AttributeName(name), AttributeLabel(label), `type`, colName)
  }

  override def getAttributeName(attribute: Attribute): AttributeName =
    getAttributeData(attribute).name

  override def getAttributeLabel(attribute: Attribute): AttributeLabel =
    getAttributeData(attribute).label

  override def getAttributeType(attribute: Attribute): DataType =
    getAttributeData(attribute).`type`

  override def getAttributeColumnName(attribute: Attribute): ColumnName =
    getAttributeData(attribute).dbColumn

}
