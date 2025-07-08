package com.folib.validation.users;



import jakarta.inject.Inject;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.util.StringUtils;

/**
 * @author Veadan
 */
public class UniqueUsernameValidator
        implements ConstraintValidator<UniqueUsername, String>
{

    @Inject
    private UserDetailsService userDetailsService;

    @Override
    public void initialize(UniqueUsername constraint)
    {
        // Empty method, not used.
    }

    @Override
    public boolean isValid(String username,
                           ConstraintValidatorContext context)
    {
        if (StringUtils.isEmpty(username))
        {
            return true;
        }
        try
        {
            userDetailsService.loadUserByUsername(username);
        }
        catch (UsernameNotFoundException e)
        {
            return true;
        }
        return false;
    }

}
