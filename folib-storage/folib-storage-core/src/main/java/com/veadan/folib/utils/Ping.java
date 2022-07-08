package com.veadan.folib.utils;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URL;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author veadan
 */
public class Ping
{

    private static final Logger logger = LoggerFactory.getLogger(Ping.class);

    public static boolean pingHost(String host,
                                   int timeoutMilliseconds)
    {
        try (Socket socket = new Socket())
        {
            final URL url = new URL(host);
            final String urlHost = url.getHost();
            final int urlPort = url.getPort() != -1 ? url.getPort() : url.getDefaultPort();
            socket.connect(new InetSocketAddress(urlHost, urlPort), timeoutMilliseconds);
            return true;
        }
        catch (IOException ex)
        {
            logger.debug("Ping failed.", ex);
            return false;
        }
    }

}
