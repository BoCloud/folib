package com.folib.ext.jersey;

import javax.inject.Singleton;

import org.glassfish.jersey.message.internal.CacheControlProvider;
import org.glassfish.jersey.message.internal.CookieProvider;
import org.glassfish.jersey.message.internal.DateProvider;
import org.glassfish.jersey.message.internal.EntityTagProvider;
import org.glassfish.jersey.message.internal.LinkProvider;
import org.glassfish.jersey.message.internal.LocaleProvider;
import org.glassfish.jersey.message.internal.MessagingBinders;
import org.glassfish.jersey.message.internal.NewCookieProvider;
import org.glassfish.jersey.message.internal.StringHeaderProvider;
import org.glassfish.jersey.message.internal.UriProvider;
import org.glassfish.jersey.spi.HeaderDelegateProvider;

/**
 * @author veadan
 *
 * @see MessagingBinders.HeaderDelegateProviders
 */
public class CustomJerseyHeaderDelegateProviders
        extends MessagingBinders.HeaderDelegateProviders
{

    @Override
    protected void configure()
    {
        bind(CacheControlProvider.class).to(HeaderDelegateProvider.class).in(Singleton.class);
        bind(CookieProvider.class).to(HeaderDelegateProvider.class).in(Singleton.class);
        bind(DateProvider.class).to(HeaderDelegateProvider.class).in(Singleton.class);
        bind(EntityTagProvider.class).to(HeaderDelegateProvider.class).in(Singleton.class);
        bind(LinkProvider.class).to(HeaderDelegateProvider.class).in(Singleton.class);
        bind(LocaleProvider.class).to(HeaderDelegateProvider.class).in(Singleton.class);
        bind(CustomJerseyMediaTypeProvider.class).to(HeaderDelegateProvider.class).in(Singleton.class);
        bind(NewCookieProvider.class).to(HeaderDelegateProvider.class).in(Singleton.class);
        bind(StringHeaderProvider.class).to(HeaderDelegateProvider.class).in(Singleton.class);
        bind(UriProvider.class).to(HeaderDelegateProvider.class).in(Singleton.class);
    }
}
