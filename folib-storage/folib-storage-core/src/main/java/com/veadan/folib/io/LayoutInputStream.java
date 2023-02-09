package com.veadan.folib.io;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.veadan.folib.util.MessageDigestUtils;
import org.apache.commons.codec.digest.MessageDigestAlgorithms;
import org.apache.commons.io.input.ProxyInputStream;

/**
 * This class decorates storage {@link InputStream} with common layout specific logic.
 * 
 * You don't need to instantiate it directly, see example below:
 * 
 * <pre>
 *     RepositoryPath repositoryPath = repositlryPathResolver.resolve("path/to/your/artifact/file.ext");
 *     ArtifactInputStream aos = (ArtifactInputStream) Files.newInputStream(repositoryPath); 
 * </pre>
 * 
 * @author mtodorov
 * 
 */
public class LayoutInputStream
        extends ProxyInputStream
{

    private static final Set<String> DEFAULT_ALGORITHM_SET = Stream.of(MessageDigestAlgorithms.MD5,
                                                                       MessageDigestAlgorithms.SHA_1)
                                                                   .collect(Collectors.toSet()); 
    
    private Map<String, MessageDigest> digests = new LinkedHashMap<>();

    private Map<String, String> hexDigests = new LinkedHashMap<>();

    public LayoutInputStream(InputStream is,
                             Set<String> checkSumDigestAlgorithmSet)
        throws NoSuchAlgorithmException
    {
        super(new BufferedInputStream(is));
        
        for (String algorithm : checkSumDigestAlgorithmSet)
        {
            addAlgorithm(algorithm);
        }
    }

    public LayoutInputStream(InputStream is)
        throws NoSuchAlgorithmException
    {
        this(is, DEFAULT_ALGORITHM_SET);
    }

    public final void addAlgorithm(String algorithm)
            throws NoSuchAlgorithmException
    {
        MessageDigest digest = MessageDigest.getInstance(algorithm);

        digests.put(algorithm, digest);
    }

    public MessageDigest getMessageDigest(String algorithm)
    {
        return digests.get(algorithm);
    }

    public Map<String, MessageDigest> getDigests()
    {
        return digests;
    }

    public void resetHexDidests()
    {
        hexDigests.clear();
    }
    
    public Map<String, String> getHexDigests()
    {
        return hexDigests;
    }

    public String getMessageDigestAsHexadecimalString(String algorithm)
    {
        if (hexDigests.containsKey(algorithm))
        {
            return hexDigests.get(algorithm);
        }
        else
        {
            // This method will invoke MessageDigest.digest() which will reset the bytes when it's done
            // and thus this data will no longer be available, so we'll need to cache the calculated digest
            String hexDigest = MessageDigestUtils.convertToHexadecimalString(getMessageDigest(algorithm));
            hexDigests.put(algorithm, hexDigest);

            return hexDigest;
        }
    }

    public void setDigests(Map<String, MessageDigest> digests)
    {
        this.digests = digests;
    }

    public Map<String, String> getDigestMap()
    {
        return hexDigests;
    }

    @Override
    public int read()
            throws IOException
    {
        int ch = in.read();
        if (ch != -1)
        {
            for (Map.Entry entry : digests.entrySet())
            {
                MessageDigest digest = (MessageDigest) entry.getValue();
                digest.update((byte) ch);
            }
        }

        return ch;
    }

    @Override
    public int read(byte[] bytes,
                    int off,
                    int len)
            throws IOException
    {
        int numberOfBytesRead = in.read(bytes, off, len);
        if (numberOfBytesRead != -1)
        {
            for (Map.Entry entry : digests.entrySet())
            {
                MessageDigest digest = (MessageDigest) entry.getValue();
                digest.update(bytes, off, numberOfBytesRead);
            }
        }

        return numberOfBytesRead;
    }

    @Override
    public int read(byte[] bytes)
            throws IOException
    {
        int len = in.read(bytes);

        for (Map.Entry entry : digests.entrySet())
        {
            MessageDigest digest = (MessageDigest) entry.getValue();
            if (len != -1) {
                digest.update(bytes, 0, len);
            }
        }

        return len;
    }

    InputStream getTarget()
    {
        return in;
    }
    
}
