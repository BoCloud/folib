package com.folib.authentication.api;

import java.io.IOException;
import java.util.function.Predicate;

import org.springframework.context.ApplicationContext;

public interface AuthenticationItemConfigurationManager
{

    void updateAuthenticationItems(AuthenticationItems items)
        throws IOException;

    AuthenticationItems getAuthenticationItems();

    <T> T getCustomAuthenticationItem(CustomAuthenticationItemMapper<T> mapper);

    <T> void putCustomAuthenticationItem(T customAuthenticationItem,
                                         CustomAuthenticationItemMapper<T> mapper)
        throws IOException;

    <T> void testCustomAuthenticationItem(T customAuthenticationItem,
                                          CustomAuthenticationItemMapper<T> mapper,
                                          Predicate<ApplicationContext> p)
        throws IOException;

}
