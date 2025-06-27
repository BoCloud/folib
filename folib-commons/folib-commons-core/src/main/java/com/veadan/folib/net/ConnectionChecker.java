package com.veadan.folib.net;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketTimeoutException;

/**
 * @author veadan
 */
public class ConnectionChecker
{


    private ConnectionChecker() 
    {
    }

    public static boolean checkServiceAvailability(String host, int port, int timeout)
            throws IOException
    {
        boolean isConnected = false;
        try (Socket socket = new Socket())
        {
            socket.connect(new InetSocketAddress(host, port), timeout);
            isConnected = socket.isConnected();
        }
        catch (SocketException e)
        {
            if (e.getMessage().contains("Connection refused"))
            {
                return false;
            }
        }
        catch (SocketTimeoutException e)
        {
            return false;
        }

        return isConnected;
    }

}
