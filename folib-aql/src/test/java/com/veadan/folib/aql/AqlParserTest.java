package com.veadan.folib.aql;

import com.veadan.folib.data.criteria.OQueryTemplate;
import com.veadan.folib.data.criteria.Predicate;
import com.veadan.folib.data.criteria.QueryParserException;
import com.veadan.folib.data.criteria.Selector;
import com.veadan.folib.aql.grammar.AqlQueryParser;
import com.veadan.folib.domain.ArtifactEntity;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.google.common.collect.ImmutableMap;
import org.javatuples.Pair;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import static org.assertj.core.api.Assertions.assertThat;

@Disabled("See ")
public class AqlParserTest
{

    private static final Logger logger = LoggerFactory.getLogger(AqlParserTest.class);


    @Test
    public void testLayoutSpecificKeywords()
    {
        String query = "storage:storage-common-proxies +repository:carlspring +groupId:'org.carlspring'" +
                       " +artifactId:'some strange group' asc: version";

        AqlQueryParser aqlParser = new AqlQueryParser(query);

        logger.debug("Query [{}] parse tree:\n[{}]", query, aqlParser);

        Selector<ArtifactEntity> selector = aqlParser.parseQuery();
        Predicate predicate = selector.getPredicate();

        assertThat(predicate).isNotNull();
        assertThat(predicate.isEmpty()).isFalse();
        assertThat(aqlParser.hasErrors()).isFalse();

        query = "storage:storage-common-proxies +repository:carlspring +invalidId:'org.carlspring'" +
                " +artifactId:'test-artifact' asc: unknownCoordinateId";

        aqlParser = new AqlQueryParser(query);

        logger.debug("Query [{}] parse tree:\n[{}]", query, aqlParser);

        Map<Pair<Integer, Integer>, String> errorMap = null;
        try
        {
            aqlParser.parseQuery();
        }
        catch (QueryParserException e)
        {
            errorMap = aqlParser.getErrors();
        }

        logger.debug("Query [{}] parse tree:\n[{}]", query, aqlParser);

        assertThat(aqlParser.hasErrors()).isTrue();
        assertThat(errorMap).isNotNull();
        assertThat(errorMap).hasSize(2);

        List<Pair<Integer, Integer>> errorPositionList = new ArrayList<>(errorMap.keySet());

        assertThat(Pair.with(1, 55).equals(errorPositionList.get(0))).isTrue();
        assertThat(Pair.with(1, 115).equals(errorPositionList.get(1))).isTrue();
    }

    @Test
    public void testValidDoubleQuotedQuerySpecificChars()
    {
        String query = "repository: \"releases#1\"";

        AqlQueryParser aqlParser = new AqlQueryParser(query);

        aqlParser.parseQuery();

        logger.debug("Query [{}] parse tree:\n[{}]", query, aqlParser);

        assertThat(aqlParser.hasErrors()).isFalse();
    }

    @Test
    public void testValidSingleQuotedQuerySpecificChars()
    {
        String query = "repository: 'releases#1'";

        AqlQueryParser aqlParser = new AqlQueryParser(query);

        aqlParser.parseQuery();

        logger.debug("Query [{}] parse tree:\n[{}]", query, aqlParser);

        assertThat(aqlParser.hasErrors()).isFalse();
    }

    @Test
    public void testInvalidUnquotedStringQuery()
    {
        String query = "repository: releases#1";

        AqlQueryParser aqlParser = new AqlQueryParser(query);
        Map<Pair<Integer, Integer>, String> errorMap = null;
        try
        {
            aqlParser.parseQuery();
        }
        catch (QueryParserException e)
        {
            errorMap = aqlParser.getErrors();
        }

        logger.debug("Query [{}] parse tree:\n[{}]", query, aqlParser);

        assertThat(aqlParser.hasErrors()).isTrue();
        assertThat(errorMap).isNotNull();

        List<Pair<Integer, Integer>> errorPositionList = new ArrayList<>(errorMap.keySet());

        assertThat(Pair.with(1, 20).equals(errorPositionList.get(0))).isTrue();
        assertThat(Pair.with(1, 21).equals(errorPositionList.get(1))).isTrue();
    }

    @Test
    public void testValidUnquotedStringQuery()
    {
        String query = "repository: releases_1";

        AqlQueryParser aqlParser = new AqlQueryParser(query);

        aqlParser.parseQuery();

        logger.debug("Query [{}] parse tree:\n[{}]", query, aqlParser);

        assertThat(aqlParser.hasErrors()).isFalse();
    }

    @Test
    @Disabled
    public void testValidQueryWithUpperLowercaseCheck()
    {
        String query = "(storagE:storage-common-proxies) +Repository:carlspring Or +(groupId:'org.carlspring')" +
                       " AnD (!(artifactId:'some strange group') || !version:0.*) aSc: agE sKip: 12";

        AqlQueryParser aqlParser = new AqlQueryParser(query);

        logger.debug("Query [{}] parse tree:\n[{}]", query, aqlParser);

        Selector<ArtifactEntity> selector = aqlParser.parseQuery();
        Predicate predicate = selector.getPredicate();

        assertThat(predicate).isNotNull();
        assertThat(predicate.isEmpty()).isFalse();
        assertThat(aqlParser.hasErrors()).isFalse();

        OQueryTemplate<Object, ArtifactEntity> queryTemplate = new OQueryTemplate<>(null);

        String sqlQuery = queryTemplate.calculateQueryString(selector);

        logger.debug("Query [{}] parse result:\n[{}]", query, sqlQuery);

        assertThat(sqlQuery)
                .isEqualTo("SELECT * " +
                           "FROM " +
                           "ArtifactEntry " +
                           "WHERE " +
                           "artifactCoordinates IS NOT NULL  " +
                           "AND ((storageId = :storageId_0) " +
                           "AND repositoryId = :repositoryId_1 " +
                           "OR (artifactCoordinates.coordinates.groupId = :groupId_1) " +
                           "AND ( NOT ((artifactCoordinates.coordinates.artifactId = :artifactId_1)) OR " +
                           " NOT (artifactCoordinates.version LIKE :version_2))) " +
                           "ORDER BY lastUpdated ASC " +
                           "SKIP 12 " +
                           "LIMIT 25");

        Map<String, Object> parameterMap = queryTemplate.exposeParameterMap(predicate);

        logger.debug("Query [{}] parse parameters:\n[{}]", query, parameterMap);

        assertThat(parameterMap)
                .isEqualTo(ImmutableMap.of("storageId_0",
                                           "storage-common-proxies",
                                           "repositoryId_1",
                                           "carlspring",
                                           "groupId_1",
                                           "org.carlspring",
                                           "version_2",
                                           "0.%",
                                           "artifactId_1",
                                           "some strange group")
                );
    }

    @Test
    public void testInvalidQuery()
    {
        String query = "[storage:storage0] ++repository0:releases ||| & groupId:org.carlspring-version:1.2.3)";

        AqlQueryParser aqlParser = new AqlQueryParser(query);
        Map<Pair<Integer, Integer>, String> errorMap = null;
        try
        {
            aqlParser.parseQuery();
        }
        catch (QueryParserException e)
        {
            errorMap = aqlParser.getErrors();
        }

        logger.debug("Query [{}] parse tree:\n[{}]", query, aqlParser);

        assertThat(aqlParser.hasErrors()).isTrue();
        assertThat(errorMap).isNotNull();
        assertThat(errorMap).hasSize(4);

        List<Pair<Integer, Integer>> errorPositionList = new ArrayList<>(errorMap.keySet());

        assertThat(Pair.with(1, 0).equals(errorPositionList.get(0))).isTrue();
        assertThat(Pair.with(1, 17).equals(errorPositionList.get(1))).isTrue();
        assertThat(Pair.with(1, 20).equals(errorPositionList.get(2))).isTrue();
        assertThat(Pair.with(1, 78).equals(errorPositionList.get(3))).isTrue();
    }

}
