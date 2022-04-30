package com.veadan.folib.controllers.support;

import java.util.List;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;

/**
 * A generic object used to produce JSON lists.
 *
 *   {
 *       "myFieldName": [
 *          your list content as json.
 *       ]
 *   }
 *
 * @author Steve Todorov
 */
@JsonSerialize(using = ListEntityBodyJsonSerializer.class)
public class ListEntityBody
{

    private String fieldName;
    private List list;

    public ListEntityBody(final String fieldName, final List list)
    {
        this.fieldName = fieldName;
        this.list = list;
    }

    public String getFieldName()
    {
        return fieldName;
    }

    public List getList()
    {
        return list;
    }
}
