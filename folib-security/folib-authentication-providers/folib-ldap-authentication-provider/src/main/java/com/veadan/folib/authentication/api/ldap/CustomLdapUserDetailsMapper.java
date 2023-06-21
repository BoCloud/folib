package com.veadan.folib.authentication.api.ldap;

import java.util.Base64;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.crypto.codec.Hex;
import org.springframework.security.crypto.codec.Utf8;
import org.springframework.security.ldap.userdetails.LdapUserDetailsMapper;
import org.springframework.util.StringUtils;

/**
 * This class handles password base64 decoding based on the property folib.authentication.ldap.userPasswordEncoded.
 *
 * <p>
 *  When set to true will handle these possible cases:
 *  {ALG}base64.encode(md5/sha1/bcrypt(mypassword))
 *  base64.encode({ALG}md5/sha1/bcrypt(mypassword))
 * <p>
 *
 * <p>
 *  When set to false will handle the ordinary case:
 *  {ALG}md5/sha1/bcrypt(mypassword)
 * </p>
 *
 * @author mbharti
 * @date 19/10/20
 */
public class CustomLdapUserDetailsMapper
        extends LdapUserDetailsMapper
{

    private static final String PREFIX = "{";

    private static final String SUFFIX = "}";

    private static final String EMPTY_STRING = "";

    private static final Logger logger = LoggerFactory.getLogger(CustomLdapUserDetailsMapper.class);

    private boolean isUserPasswordEncoded;

    @Override
    protected String mapPassword(Object passwordValue)
    {
        String passwordValueString = super.mapPassword(passwordValue);

        if (!isUserPasswordEncoded())
        {
            return passwordValueString;
        }

        return decodeBase64EncodedPassword(passwordValueString);
    }

    private String decodeBase64EncodedPassword(String prefixEncodedPasswordString)
    {
        try
        {
            String algorithmUsed = extractId(prefixEncodedPasswordString);
            String extractBase64EncodedHash = prefixEncodedPasswordString;

            if (!StringUtils.isEmpty(algorithmUsed))
            {
                extractBase64EncodedHash = extractEncodedPassword(prefixEncodedPasswordString);

                return PREFIX + algorithmUsed + SUFFIX + decodeBase64EncodedHashWithHex(extractBase64EncodedHash);
            }
            else
            {
                return new String(Base64.getDecoder().decode(Utf8.encode(extractBase64EncodedHash)));
            }
        }
        catch (Exception e)
        {
            logger.warn("Failed to match password after decoding base64encoded hash after algorithm", e);

            return prefixEncodedPasswordString;
        }
    }

    private String decodeBase64EncodedHashWithHex(String base64EncodedHash)
    {
        try
        {
            return new String(Hex.encode(Base64.getDecoder().decode(Utf8.encode(base64EncodedHash))));
        }
        catch (Exception ex)
        {
            logger.warn("decode hash using base64! " + ex.getMessage(), ex);
        }

        return base64EncodedHash;
    }

    private String extractEncodedPassword(String prefixEncodedPassword)
    {
        int start = prefixEncodedPassword.indexOf(SUFFIX);

        return prefixEncodedPassword.substring(start + 1);
    }

    private String extractId(String prefixEncodedPassword)
    {
        int start = prefixEncodedPassword.indexOf(PREFIX);

        if (start != 0)
        {
            return EMPTY_STRING;
        }

        int end = prefixEncodedPassword.indexOf(SUFFIX, start);

        if (end < 0)
        {
            return EMPTY_STRING;
        }

        return prefixEncodedPassword.substring(start + 1, end);
    }


    /**
     * Getting value whether Base64EncodedPassword is enabled or not
     *
     * @return boolean
     */
    public boolean isUserPasswordEncoded()
    {
        return isUserPasswordEncoded;
    }


    /**
     * Setting value whether Base64EncodedPassword is enabled or not
     *
     * @param userPasswordEncoded
     */
    public void setUserPasswordEncoded(boolean userPasswordEncoded)
    {
        isUserPasswordEncoded = userPasswordEncoded;
    }
}
