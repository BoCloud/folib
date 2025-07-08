package com.folib.aql.grammar;

import java.util.Optional;

import com.folib.artifact.coordinates.ArtifactLayoutLocator;
import com.folib.data.criteria.DefaultExpressionDialect;
import com.folib.data.criteria.Expression;
import com.folib.data.criteria.QueryParserException;
import org.springframework.util.Assert;

/**
 * This class used to provice AQL specific {@link Expression} parseing.
 * 
 * @author veadan
 *
 */
public class AqlExpressionDialect extends DefaultExpressionDialect
{

    /**
     * AQL keyword mapping
     */
    private AqlMapping keyword;

    /**
     * Expression value.
     */
    private String value;

    /**
     * Binary flags to determine parsing position in current expression.
     * We need to be sure that we move step by step correctly: parse expression
     * property (01) -> parse expression value (10) -> parse expression operator
     * (11)
     */
    private int state = 0;

    @Override
    public String parseValue(String value)
    {
        Assert.state(state == 1, "You should process property first.");

        state = state | 2;
        String result = super.parseValue(value);
        this.value = probeForWildcardValue(result).map(v -> v.replaceAll("\\*", "%"))
                                                  .orElse(result);
        if (AqlMapping.LAYOUT.equals(keyword))
        {
            this.value = ArtifactLayoutLocator.getLayoutEntityMap()
                                              .entrySet()
                                              .stream()
                                              .filter(e -> e.getKey()
                                                            .equals(this.value))
                                              .map(e -> e.getValue())
                                              .findFirst()
                                              .orElseThrow(() -> new QueryParserException(
                                                      String.format("Unknown layout [%s].",
                                                                    value)))
                                              .getArtifactCoordinatesClass()
                                              .getSimpleName();

        }

        return this.value;
    }

    @Override
    public String parseProperty(String attribute)
    {
        Assert.state(state == 0,
                     String.format("Dirty parse context, you should use new [%s] instance to start parse new expression.",
                                   AqlExpressionDialect.class.getSimpleName()));

        state = state | 1;
        for (AqlMapping aqlKeyword : AqlMapping.values())
        {
            if (!aqlKeyword.toString().equalsIgnoreCase(attribute))
            {
                continue;
            }
            this.keyword = aqlKeyword;
            return aqlKeyword.property();
        }
        return String.format("artifactCoordinates.coordinates.%s",
                             attribute);
    }

    @Override
    public Expression.ExpOperator parseOperator(String operator)
    {
        Assert.state(state == 3, "You should process property and value first.");

        if (AqlMapping.FROM.equals(keyword))
        {
            return Expression.ExpOperator.GE;
        }
        else if (AqlMapping.TO.equals(keyword))
        {
            return Expression.ExpOperator.LE;
        }
        else if (AqlMapping.AGE.equals(keyword))
        {
            return Expression.ExpOperator.LE;
        }
        else if (value != null && value.contains("%"))
        {
            return Expression.ExpOperator.LIKE;
        }
        return Expression.ExpOperator.EQ;
    }

    private Optional<String> probeForWildcardValue(String value)
    {
        return Optional.ofNullable(value)
                       .filter(v -> v.startsWith("*") || v.endsWith("*"));
    }

}
