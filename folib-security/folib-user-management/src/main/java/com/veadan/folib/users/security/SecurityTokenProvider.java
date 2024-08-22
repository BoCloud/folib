package com.veadan.folib.users.security;

import com.veadan.folib.security.exceptions.ExpiredTokenException;
import com.veadan.folib.security.exceptions.InvalidTokenException;
import com.veadan.folib.users.service.AccessTokenFinder;
import org.jose4j.jws.AlgorithmIdentifiers;
import org.jose4j.jws.JsonWebSignature;
import org.jose4j.jwt.JwtClaims;
import org.jose4j.jwt.MalformedClaimException;
import org.jose4j.jwt.NumericDate;
import org.jose4j.jwt.consumer.InvalidJwtException;
import org.jose4j.jwt.consumer.JwtConsumer;
import org.jose4j.jwt.consumer.JwtConsumerBuilder;
import org.jose4j.keys.HmacKey;
import org.jose4j.lang.JoseException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
import javax.inject.Inject;
import java.io.UnsupportedEncodingException;
import java.security.Key;
import java.util.Calendar;
import java.util.Locale;
import java.util.Map;

/**
 * Used to get and verify security tokens. <br>
 * This implementation based on JSON Web Token (JWT) which is RFC 7519 standard. <br>
 *
 * @author @author veadan
 */
@Component
public class SecurityTokenProvider
{
    private static final String MESSAGE_INVALID_JWT = "Invalid JWT: value-[%s]";

    private static final Integer DEFAULT_EXPIRE_SECONDS = 7200;
    /**
     * Secret key which is used to encode and verify tokens.<br>
     * All previous tokens will be invalid, if it changed.
     */
    private Key key;

    @Resource
    private AccessTokenFinder accessTokenFinder;

    /**
     * Creates {@link Key} instance using Secret string from application configuration.
     *
     * @param secret
     * @throws UnsupportedEncodingException
     * @throws Exception
     */
    @Inject
    public void init(@Value("${folib.security.jwtSecret:secret}") String secret)
            throws UnsupportedEncodingException
    {
        key = new HmacKey(secret.getBytes("UTF-8"));
    }

    /**
     * Generates an encrypted token.
     *
     * @param subject       a Subject which is used as token base.
     * @param claimMap      an additional Claims which will also present in token.
     * @param expireSeconds
     * @param issuedAt
     * @return encrypted token string.
     * @throws JoseException
     */
    public String getToken(String subject,
                           Map<String, String> claimMap,
                           Integer expireSeconds,
                           NumericDate issuedAt)
            throws JoseException
    {
        JwtClaims claims = new JwtClaims();
        claims.setIssuer("Folib");
        claims.setGeneratedJwtId();
        claims.setSubject(subject);
        claimMap.entrySet().stream().forEach((e) ->
                                             {
                                                 claims.setClaim(e.getKey(), e.getValue());
                                             });

        handleExpiration(expireSeconds, claims);

        JsonWebSignature jws = new JsonWebSignature();
        jws.setPayload(claims.toJson());
        jws.setKey(key);
        jws.setDoKeyValidation(false);
        jws.setAlgorithmHeaderValue(AlgorithmIdentifiers.HMAC_SHA256);

        return jws.getCompactSerialization();
    }

    private static void handleExpiration(Integer expireSeconds, JwtClaims claims) {
        Calendar issuedCalendar = Calendar.getInstance(Locale.ROOT);
        Calendar expirationCalendar = Calendar.getInstance(Locale.ROOT);
        expirationCalendar.setTime(issuedCalendar.getTime());
        expireSeconds = expireSeconds == null ? DEFAULT_EXPIRE_SECONDS : expireSeconds;
        expirationCalendar.add(Calendar.SECOND, expireSeconds);

        claims.setIssuedAt(NumericDate.fromMilliseconds(issuedCalendar.getTimeInMillis()));
        claims.setExpirationTime(NumericDate.fromMilliseconds(expirationCalendar.getTimeInMillis()));
    }

    public String getSubject(String token)
    {

        JwtClaims jwtClaims = getClaims(token);
        String subject;
        try
        {
            subject = jwtClaims.getSubject();
        }
        catch (MalformedClaimException e)
        {
            throw new InvalidTokenException(String.format(MESSAGE_INVALID_JWT, token), e);
        }
        return subject;
    }

    public JwtClaims getClaims(String token)
    {
        return getClaims(token, false);
    }
    
    public JwtClaims getClaims(String token, boolean verify)
    {
        JwtConsumerBuilder builder = new JwtConsumerBuilder().setRequireSubject()
                                                             .setRelaxVerificationKeyValidation();
        if (!verify)
        {
            builder.setSkipSignatureVerification().setSkipAllValidators();
        }
        else
        {
            builder.setVerificationKey(key);
        }
        
        JwtConsumer jwtConsumer = builder.build();

        JwtClaims jwtClaims;
        try
        {
            jwtClaims = jwtConsumer.processToClaims(token);
        }
        catch (InvalidJwtException e)
        {
            if (e.getMessage().contains("The JWT is no longer valid"))
            {
                throw new ExpiredTokenException(String.format(MESSAGE_INVALID_JWT, token), e);
            }
            throw new InvalidTokenException(String.format(MESSAGE_INVALID_JWT, token), e);
        }
        return jwtClaims;
    }

    /**
     * @param token
     * @param targetSubject
     * @param targetClaimMap
     */
    public void verifyToken(String token,
                            String targetSubject,
                            Map<String, String> targetClaimMap)
    {
        JwtClaims jwtClaims = getClaims(token, true);
        String subject;
        String jwtId;
        try
        {
            subject = jwtClaims.getSubject();
            jwtId=jwtClaims.getJwtId();
        }
        catch (MalformedClaimException e)
        {
            throw new InvalidTokenException(String.format(MESSAGE_INVALID_JWT, token), e);
        }

        if (!targetSubject.equals(subject))
        {
            throw new InvalidTokenException(String.format(MESSAGE_INVALID_JWT, token));
        }

        boolean claimMatch;
        try
        {
            claimMatch = targetClaimMap.entrySet()
                                       .stream()
                                       .allMatch((e) -> e.getValue().equals(jwtClaims.getClaimValue(e.getKey())));
        }
        catch (Exception e)
        {
            throw new InvalidTokenException(String.format(MESSAGE_INVALID_JWT, token), e);
        }

        if (!claimMatch)
        {
            throw new InvalidTokenException(String.format(MESSAGE_INVALID_JWT, token));
        }
        if("1".equals(jwtClaims.getClaimValue("pac"))){
            if(!accessTokenFinder.getByJwtId(jwtId)){
                throw new InvalidTokenException(String.format(MESSAGE_INVALID_JWT, token));
            }
        }
    }


    public String getAccessToken(String subject,
                                 Map<String, String> claimMap,
                                 Long end,String jid)
            throws JoseException {
        JwtClaims claims = new JwtClaims();
        claims.setIssuer("Folib");
        claims.setJwtId(jid);
        claims.setSubject(subject);
        claimMap.entrySet().forEach((e) ->
        {
            claims.setClaim(e.getKey(), e.getValue());
        });

        long start = System.currentTimeMillis();
        claims.setIssuedAt(NumericDate.fromMilliseconds(start));
        NumericDate expireTime=end==null?null:NumericDate.fromMilliseconds(end);
        claims.setExpirationTime(expireTime);
        JsonWebSignature jws = new JsonWebSignature();
        jws.setPayload(claims.toJson());
        jws.setKey(key);
        jws.setDoKeyValidation(false);
        jws.setAlgorithmHeaderValue(AlgorithmIdentifiers.HMAC_SHA256);

        return jws.getCompactSerialization();
    }

}
