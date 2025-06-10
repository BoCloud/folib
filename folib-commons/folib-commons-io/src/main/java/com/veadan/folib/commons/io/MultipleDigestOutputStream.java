package com.veadan.folib.commons.io;

import com.veadan.folib.commons.encryption.EncryptionAlgorithmsEnum;
import com.veadan.folib.commons.util.MessageDigestUtils;

import java.io.*;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author mtodorov
 */
public class MultipleDigestOutputStream extends FilterOutputStream
{

    private static final String[] DEFAULT_ALGORITHMS = { EncryptionAlgorithmsEnum.MD5.getAlgorithm(),
                                                         EncryptionAlgorithmsEnum.SHA1.getAlgorithm(),
                                                         EncryptionAlgorithmsEnum.SHA256.getAlgorithm(),
                                                         EncryptionAlgorithmsEnum.SHA512.getAlgorithm() };

    private Map<String, MessageDigest> digests = new LinkedHashMap<>();

    private Map<String, String> hexDigests = new LinkedHashMap<>();

    private Path path;

    private boolean generateChecksumFiles;

    private DigestUpdater digestUpdater = new DigestUpdater();


    public MultipleDigestOutputStream(OutputStream os)
            throws NoSuchAlgorithmException
    {
        super(os);

        addAlgorithms(DEFAULT_ALGORITHMS);
    }

    public MultipleDigestOutputStream(OutputStream os, String[] algorithms)
            throws NoSuchAlgorithmException
    {
        super(os);

        addAlgorithms(algorithms);
    }

    public MultipleDigestOutputStream(File file, OutputStream os)
            throws NoSuchAlgorithmException
    {
        this(Paths.get(file.getAbsolutePath()), os, DEFAULT_ALGORITHMS, true);
    }

    public MultipleDigestOutputStream(Path path, OutputStream os)
            throws NoSuchAlgorithmException
    {
        this(path, os, DEFAULT_ALGORITHMS, true);
    }

    public MultipleDigestOutputStream(Path path,
                                      OutputStream os,
                                      String[] algorithms,
                                      boolean generateChecksumFiles)
            throws NoSuchAlgorithmException
    {
        super(os);

        this.path = path;
        this.generateChecksumFiles = generateChecksumFiles;

        addAlgorithms(algorithms);
    }

    private void addAlgorithms(String[] algorithms)
            throws NoSuchAlgorithmException
    {
        for (String algorithm : algorithms)
        {
            addAlgorithm(algorithm);
        }
    }

    public void addAlgorithm(String algorithm)
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

    @Override
    public void write(int b)
            throws IOException
    {
        out.write(b);

    }

    @Override
    public void write(byte[] b)
            throws IOException
    {
        out.write(b);

//        DigestUpdater.updateDigestsConcurrently(digests, b);
        digestUpdater.updateDigestsConcurrently(digests, b);
    }

    @Override
    public void write(byte[] b, int off, int len)
            throws IOException
    {
        out.write(b, off, len);

//        DigestUpdater.updateDigestsConcurrently(digests, b, off, len);
        digestUpdater.updateDigestsConcurrently(digests, b, off, len);
    }

    @Override
    public void close()
            throws IOException
    {
        super.close();

        if (generateChecksumFiles)
        {
            writeChecksums();
        }

        digestUpdater.clearThreadPool();
    }

    public void writeChecksums()
            throws IOException
    {
        for (Map.Entry entry : digests.entrySet())
        {
            MessageDigest digest = (MessageDigest) entry.getValue();

            String hexDigest = getMessageDigestAsHexadecimalString(digest.getAlgorithm());

            writeChecksum(path, digest.getAlgorithm(), hexDigest);
        }
    }

    private void writeChecksum(Path path, String algorithm, String hexDigest)
            throws IOException
    {
        final String filePathStr =
                path.toAbsolutePath().toString() + EncryptionAlgorithmsEnum.fromAlgorithm(algorithm).getExtension();

        try (FileWriter fw = new FileWriter(filePathStr))
        {
            fw.write(hexDigest + "\n");
            fw.flush();
        }
    }

    public Path getPath()
    {
        return path;
    }

    public void setPath(Path path)
    {
        this.path = path;
    }

    public boolean isGenerateChecksumFiles()
    {
        return generateChecksumFiles;
    }

    public void setGenerateChecksumFiles(boolean generateChecksumFiles)
    {
        this.generateChecksumFiles = generateChecksumFiles;
    }

}
