package com.folib.data.criteria;

import com.folib.data.criteria.Expression.ExpOperator;
import com.folib.data.domain.DomainObject;

/**
 * @author veadan
 *
 * @param <T>
 */
public interface ExpressionDialect
{

    <T extends DomainObject> String parseProperty(String attribute);

    ExpOperator parseOperator(String operator);

    String parseValue(String value);

}
