<?xml version="1.0" encoding="UTF-8" ?>

<!-- New document created with EditiX at Fri Apr 05 21:50:25 CEST 2013 -->

<xsl:stylesheet version="2.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
	xmlns:xs="http://www.w3.org/2001/XMLSchema"
	xmlns:fn="http://www.w3.org/2005/xpath-functions"
	xmlns:xdt="http://www.w3.org/2005/xpath-datatypes"
	xmlns:err="http://www.w3.org/2005/xqt-errors"
	exclude-result-prefixes="xs xdt err fn">

	<xsl:output method="xml" indent="no"/>
	
	<xsl:template match="/">
		<xsl:apply-templates select="/TEI/text/body"/>
	</xsl:template>
	
	<xsl:template match="body">
		<xsl:element name="plenary_protocol">
      <xsl:attribute name="lp" select="/TEI/teiHeader/fileDesc/titleStmt/legislativePeriod/text()"></xsl:attribute>
      <xsl:attribute name="protocol_no" select="/TEI/teiHeader/fileDesc/titleStmt/sessionNo/text()"></xsl:attribute>
  		<xsl:attribute name="date" select="/TEI/teiHeader/fileDesc/publicationStmt/date/text()"/>
			<xsl:attribute name="year" select="tokenize(/TEI/teiHeader/fileDesc/publicationStmt/date/text(), '-')[1]"/>
			<xsl:attribute name="birthday" select="/TEI/teiHeader/fileDesc/editionStmt/edition/birthday/text()"/>
			<xsl:attribute name="version" select="/TEI/teiHeader/fileDesc/editionStmt/edition/version/text()"/>
			<xsl:attribute name="url" select="/TEI/teiHeader/fileDesc/sourceDesc/url/text()"/>
			<xsl:attribute name="filetype" select="/TEI/teiHeader/fileDesc/sourceDesc/filetype/text()"/>
			<xsl:apply-templates select=".//sp"/>
		</xsl:element>
	</xsl:template>

	<xsl:template match="sp">
		<xsl:variable name="speech_no">
				<xsl:number/>
		</xsl:variable>
		<xsl:for-each-group select="./node()" group-adjacent="if (self::stage) then 2 else if (self::speaker) then 3 else 1">
			<xsl:choose>					
				<xsl:when test="current-grouping-key() = 1">
					<xsl:variable name="section_no">
						<xsl:number/>
					</xsl:variable>
					<xsl:element name="speaker">
						<xsl:attribute name="id" select="concat(/TEI/teiHeader/fileDesc/titleStmt/legislativePeriod/text(), '_', /TEI/teiHeader/fileDesc/titleStmt/sessionNo/text(), '_S_', $speech_no, '_', $section_no)"></xsl:attribute>
						<xsl:attribute name="type"  select="'speech'"> </xsl:attribute>
						<xsl:attribute name="lp" select="/TEI/teiHeader/fileDesc/titleStmt/legislativePeriod/text()"></xsl:attribute>
						<xsl:attribute name="protocol_no" select="/TEI/teiHeader/fileDesc/titleStmt/sessionNo/text()"></xsl:attribute>
						<xsl:attribute name="date" select="/TEI/teiHeader/fileDesc/publicationStmt/date/text()"/>
						<xsl:attribute name="year" select="tokenize(/TEI/teiHeader/fileDesc/publicationStmt/date/text(), '-')[1]"/>
						<xsl:attribute name="ai_no" select="../../@n"/>
            <xsl:attribute name="ai_id" select="concat(/TEI/teiHeader/fileDesc/titleStmt/legislativePeriod/text(), '_', /TEI/teiHeader/fileDesc/titleStmt/sessionNo/text(), '_', ../../@n)"/>
            <xsl:attribute name="ai_type" select="../../@what"/>
						<xsl:attribute name="who" select="../@who"/>
						<xsl:attribute name="name" select="../@name"/>
						<xsl:attribute name="parliamentary_group" select="../@parliamentary_group"/>
						<xsl:attribute name="party" select="../@party"/>
						<xsl:attribute name="role" select="../@role"/>
							<xsl:for-each select="current-group()">
								<xsl:copy-of select="."></xsl:copy-of>
							</xsl:for-each>
					</xsl:element>
				</xsl:when>
				<xsl:when test="current-grouping-key() = 2">
					<xsl:variable name="interjection_no">
						<xsl:number/>
					</xsl:variable>
					<xsl:element name="speaker">
						<xsl:attribute name="id" select="concat(/TEI/teiHeader/fileDesc/titleStmt/legislativePeriod/text(), '_', /TEI/teiHeader/fileDesc/titleStmt/sessionNo/text(), '_I_', $speech_no, '_', $interjection_no)"></xsl:attribute>
						<xsl:attribute name="type"  select="'interjection'"> </xsl:attribute>
						<xsl:attribute name="lp" select="/TEI/teiHeader/fileDesc/titleStmt/legislativePeriod/text()"></xsl:attribute>
						<xsl:attribute name="protocol_no" select="/TEI/teiHeader/fileDesc/titleStmt/sessionNo/text()"></xsl:attribute>
						<xsl:attribute name="date" select="/TEI/teiHeader/fileDesc/publicationStmt/date/text()"></xsl:attribute>
						<xsl:attribute name="year" select="tokenize(/TEI/teiHeader/fileDesc/publicationStmt/date/text(), '-')[1]"></xsl:attribute>
						<xsl:attribute name="who" select="../@who"/>
						<xsl:attribute name="name" select="../@name"/>
						<xsl:attribute name="parliamentary_group" select="../@parliamentary_group"/>
						<xsl:attribute name="party" select="../@party"/>
						<xsl:attribute name="role" select="../@role"/>

				<xsl:value-of select="."></xsl:value-of>

					</xsl:element>
				</xsl:when>
				<xsl:when test="current-grouping-key() = 3">
					<xsl:call-template name="dummy"/>
				</xsl:when>
				<xsl:when test="current-grouping-key() = 4">
					<xsl:call-template name="dummy"/>
				</xsl:when>
			</xsl:choose>
		</xsl:for-each-group>

	</xsl:template>


	<xsl:template name="dummy"/>

</xsl:stylesheet>
