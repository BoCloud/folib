package com.veadan.folib.data.criteria;

import com.veadan.folib.data.criteria.Expression.ExpOperator;
import com.veadan.folib.data.domain.DomainObject;

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
