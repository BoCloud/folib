package com.veadan.folib.validation;

import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;
import java.net.URISyntaxException;

import org.apache.commons.lang.StringUtils;

/**
 * @author Przemyslaw Fusik
 */
public class LdapUriValidator
        implements ConstraintValidator<LdapUri, String>
{

    @Override
    public void initialize(LdapUri constraint)
    {
        // empty by design
    }

    @Override
    public boolean isValid(String value,
                           ConstraintValidatorContext context)
    {
        if (StringUtils.isEmpty(value))
        {
            return true;
        }

        try
        {
            new java.net.URI(value);
        }
        catch (URISyntaxException e)
        {
            return false;
        }
        return (value.startsWith("ldap:") || value.startsWith("ldaps:"));
    }

}
