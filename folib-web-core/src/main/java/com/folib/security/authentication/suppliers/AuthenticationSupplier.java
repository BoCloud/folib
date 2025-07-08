package com.folib.security.authentication.suppliers;

import javax.annotation.CheckForNull;
import javax.annotation.Nonnull;

import jakarta.servlet.http.HttpServletRequest;
import org.springframework.security.core.Authentication;

/**
 * @author veadan
 */
public interface AuthenticationSupplier
{

    /**
     * Attempts to supply the {@link Authentication} object from the currently served HTTP request.
     */
    @CheckForNull
    Authentication supply(@Nonnull HttpServletRequest request);

    default boolean supports(@Nonnull HttpServletRequest request)
    {
        return true;
    }

}
