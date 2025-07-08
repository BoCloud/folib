package com.folib.data.criteria;

import org.apache.commons.lang3.StringUtils;
import com.folib.data.criteria.Expression.ExpOperator;

public class DefaultExpressionDialect implements ExpressionDialect
{

    @Override
    public String parseProperty(String attribute)
    {
        return attribute;
    }

    @Override
    public ExpOperator parseOperator(String operator)
    {
        return ExpOperator.valueOf(operator);
    }

    @Override
    public String parseValue(String value)
    {
        String result = StringUtils.unwrap(value, "'");
        result = StringUtils.unwrap(result, '"');
        result = StringUtils.unwrap(result, '`');
        result = StringUtils.unwrap(result, '!');
        return result;
    }

}
