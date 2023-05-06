package com.veadan.folib.data.criteria;

import java.util.HashMap;
import java.util.Map;

import javax.persistence.EntityManager;

import com.veadan.folib.data.criteria.Expression.ExpOperator;
import com.veadan.folib.data.domain.DomainObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * {@link QueryTemplate} implementation for OrientDB engine.
 * 
 * @author xuxinping
 *
 */
public class OQueryTemplate<R, T extends DomainObject> implements QueryTemplate<R, T>
{
    private static final Logger logger = LoggerFactory.getLogger(OQueryTemplate.class);

    protected EntityManager entityManager;

    public OQueryTemplate()
    {
        super();
    }

    public OQueryTemplate(EntityManager entityManager)
    {
        super();
        this.entityManager = entityManager;
    }

    public EntityManager getEntityManager()
    {
        return entityManager;
    }

    public R select(Selector<T> s)
    {
        String sQuery = calculateQueryString(s);

        Map<String, Object> parameterMap = exposeParameterMap(s.getPredicate());

        logger.info("Executing SQL query:\n" +
                     "\t[{}]\n" +
                     "With parameters:\n" +
                     "\t[{}]",
                     sQuery, parameterMap);
        return null;
    }

    public Map<String, Object> exposeParameterMap(Predicate p)
    {
        return exposeParameterMap(p, 0);
    }

    private Map<String, Object> exposeParameterMap(Predicate p,
                                                   int tokenCount)
    {
        HashMap<String, Object> result = new HashMap<>();
        Expression e = p.getExpression();
        if (e != null && !ExpOperator.IS_NULL.equals(e.getOperator()) && !ExpOperator.IS_NOT_NULL.equals(e.getOperator()))
        {
            result.put(calculateParameterName(e.getProperty(), tokenCount), e.getValue());
        }

        for (Predicate predicate : p.getChildPredicateList())
        {
            result.putAll(exposeParameterMap(predicate, tokenCount++));
        }

        return result;
    }

    public String calculateQueryString(Selector<T> selector)
    {
        StringBuilder sb = new StringBuilder();
        sb.append("SELECT ").append(selector.getProjection());
        sb.append(" FROM ").append(selector.getTargetClass().getSimpleName());

        Predicate p = selector.getPredicate();
        if (p.isEmpty())
        {
            return sb.toString();
        }

        sb.append(" WHERE ");
        sb.append(predicateToken(p, 0));

        Paginator paginator = selector.getPaginator();
        if (paginator != null && paginator.getProperty() != null && !paginator.getProperty().trim().isEmpty())
        {
            sb.append(String.format(" ORDER BY %s %s", paginator.getProperty(), paginator.getOrder()));
        }

        if (paginator != null && paginator.getSkip() > 0)
        {
            sb.append(String.format(" SKIP %s", paginator.getSkip()));
        }
        if (paginator != null && paginator.getLimit() > 0)
        {
            sb.append(String.format(" LIMIT %s", paginator.getLimit()));
        }

        if (selector.isFetch())
        {
            sb.append(" FETCHPLAN *:-1");
        }

        return sb.toString();
    }

    protected String predicateToken(Predicate p,
                                    int tokenCount)
    {
        if (p.isEmpty())
        {
            return "";
        }

        StringBuffer sb = new StringBuffer();
        if (p.getExpression() != null)
        {
            sb.append(expressionToken(p.getExpression(), tokenCount));
        }

        for (Predicate predicate : p.getChildPredicateList())
        {
            if (sb.length() > 0)
            {
                sb.append(String.format(" %s ", p.getOperator().name()));
            }
            sb.append(predicateToken(predicate, tokenCount++));
        }

        if (p.isNested())
        {
            sb.insert(0, "(").append(")");
        }

        if (p.isNegated())
        {
            sb.insert(0, " NOT (").append(")");
        }

        return sb.toString();
    }

    protected String expressionToken(Expression e,
                                     int n)
    {
        String experssionLeft = expressionLeftToken(e);
        String operator = expressionOperatorToken(e);
        String expressionRight = expressionRightToken(e, n);

        return new StringBuffer().append(experssionLeft)
                                 .append(operator)
                                 .append(expressionRight)
                                 .toString();
    }

    protected String expressionLeftToken(Expression e)
    {
        switch (e.getOperator())
        {
        case CONTAINS:
            String property = e.getProperty();
            return property.substring(0, property.indexOf("."));
        default:
            break;
        }
        return e.getProperty();
    }

    protected String expressionRightToken(Expression e,
                                          int n)
    {
        switch (e.getOperator())
        {
        case CONTAINS:
            String property = e.getProperty();
            property = property.substring(property.indexOf(".") + 1);

            return String.format("(%s = :%s)", property, calculateParameterName(property, n));
        case IS_NULL:
        case IS_NOT_NULL:
            
            return "";
        default:
            break;
        }
        String property = e.getProperty();
        return String.format(":%s", calculateParameterName(property, n));
    }

    private String calculateParameterName(String property,
                                          int n)
    {
        if (property == null)
        {
            return "";
        }
        property = property.replace(".toLowerCase()", "");
        property = property.replace("@", "");
        return String.format("%s_%s", property.substring(property.lastIndexOf(".") + 1), n);
    }

    protected String expressionOperatorToken(Expression e)
    {
        switch (e.getOperator())
        {
        case EQ:
            return " = ";
        case LE:
            return " <= ";
        case GE:
            return " >=";            
        case LIKE:
            return " LIKE ";
        case CONTAINS:
            return " CONTAINS ";
        case IS_NULL:
            return " IS NULL ";
        case IS_NOT_NULL:
            return " IS NOT NULL ";            
        }
        return null;
    }
}
