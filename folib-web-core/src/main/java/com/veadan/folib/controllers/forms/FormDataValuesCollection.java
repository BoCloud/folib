package com.veadan.folib.controllers.forms;

import java.util.Collection;

/**
 * @author veadan
 */
public class FormDataValuesCollection
{

    private Collection<FormDataValues<?>> formDataValues;


    public FormDataValuesCollection(final Collection<FormDataValues<?>> formDataValues)
    {
        this.formDataValues = formDataValues;
    }

    public Collection<FormDataValues<?>> getFormDataValues()
    {
        return formDataValues;
    }

    public void setFormDataValues(final Collection<FormDataValues<?>> formDataValues)
    {
        this.formDataValues = formDataValues;
    }


}
