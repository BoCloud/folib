package com.folib.validation.users;



import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.commons.lang.StringUtils;
import org.slf4j.helpers.MessageFormatter;

public class PasswordValidator
        implements ConstraintValidator<Password, String>
{

    private Password constraint;

    @Override
    public void initialize(Password constraint)
    {
        this.constraint = constraint;
    }

    @Override
    public boolean isValid(String password,
                           ConstraintValidatorContext context)
    {
        if (StringUtils.isBlank(password))
        {
            if (constraint.allowNull())
            {
                return true;
            }
            else
            {
                context.disableDefaultConstraintViolation();
                context.buildConstraintViolationWithTemplate(constraint.message())
                       .addConstraintViolation();

                return false;
            }
        }

        boolean isValid = true;
        int length = password.length();
        if (length < constraint.min())
        {
            isValid = false;
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate(
                    format(constraint.minMessage(), constraint.min())
            ).addConstraintViolation();
        }
        else if (length > constraint.max())
        {
            isValid = false;
            context.disableDefaultConstraintViolation();
            context.buildConstraintViolationWithTemplate(
                    format(constraint.maxMessage(), constraint.max())
            ).addConstraintViolation();
        }

        return isValid;
    }

    public static String format(String msg, Object... objs) {
        return MessageFormatter.arrayFormat(msg, objs).getMessage();
    }

}
