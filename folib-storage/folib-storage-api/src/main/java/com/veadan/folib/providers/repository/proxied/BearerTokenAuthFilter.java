package com.veadan.folib.providers.repository.proxied;


import javax.ws.rs.client.ClientRequestContext;
import javax.ws.rs.client.ClientRequestFilter;
import javax.ws.rs.container.PreMatching;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.ext.Provider;
import java.io.IOException;

@Provider
@PreMatching
public class BearerTokenAuthFilter implements ClientRequestFilter {

    private final String token;

    public BearerTokenAuthFilter(String token) {
        this.token = token;
    }

    @Override
    public void filter(ClientRequestContext requestContext) throws IOException {
        if (token != null && !token.isEmpty()) {
            requestContext.getHeaders().add(HttpHeaders.AUTHORIZATION, "Bearer " + token);
        }
    }
}
