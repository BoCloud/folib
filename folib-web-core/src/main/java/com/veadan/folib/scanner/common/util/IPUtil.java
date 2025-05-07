package com.veadan.folib.scanner.common.util;

import org.springframework.http.server.reactive.ServerHttpRequest;

import jakarta.servlet.http.HttpServletRequest;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.HashSet;
import java.util.Set;

public class IPUtil {
    /**
     * 判断 ip 是否在某一个 ip 地址段内
     * e.g.
     * isInRange("10.193.0.89", "10.193.0.0/24") is true
     * isInRange("10.193.0.89", "10.193.0.61/27") is false
     *
     * @param ip
     * @param cidr
     * @return
     */

    private static final String UNKNOWN = "unknown";
    private static final String LOCALHOST = "127.0.0.1";
    private static final String SEPARATOR = ",";


    public static boolean isInRange(String ip, String cidr) {
        String[] ips = ip.split("\\.");
        int ipAddr = (Integer.parseInt(ips[0]) << 24)
                | (Integer.parseInt(ips[1]) << 16)
                | (Integer.parseInt(ips[2]) << 8) | Integer.parseInt(ips[3]);
        int type = Integer.parseInt(cidr.replaceAll(".*/", ""));
        int mask = 0xFFFFFFFF << (32 - type);
        String cidrIp = cidr.replaceAll("/.*", "");
        String[] cidrIps = cidrIp.split("\\.");
        int cidrIpAddr = (Integer.parseInt(cidrIps[0]) << 24)
                | (Integer.parseInt(cidrIps[1]) << 16)
                | (Integer.parseInt(cidrIps[2]) << 8)
                | Integer.parseInt(cidrIps[3]);

        return (ipAddr & mask) == (cidrIpAddr & mask);
    }


    public static String getDirectIp(ServerHttpRequest request) {
        return request.getRemoteAddress().getAddress().getHostAddress();
    }

    /**
     * 获取真实的 ip 地址
     * 1. request.getRemoteAddr() 获取到的是客户端的真实 ip，无法作伪；而请求头中的 X-Forwarded-For 可以伪造，并不安全。
     * 2. 如果 web 应用前放置了反向代理服务器，比如 nginx，那么通过 getRemoteAddr() 获取到的是反向代理服务器的 ip。
     * 3. 可以在 nginx 等反向代理服务器中设置 proxy_set_header X-Real-Ip $remote_addr; 即 nginx 把获取到的客户端真实 ip 放入请求头中。
     */
    public static String getRealIp(ServerHttpRequest request) {
        String ip = request.getHeaders().getFirst("X-Real-IP");
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            return getDirectIp(request);
        }

        return ip;
    }

    public static Set<String> inRange(String ip, Set<String> cidrSet) {
        if (cidrSet == null) {
            return null;
        }

        Set<String> set = new HashSet<>();
        for (String cidr : cidrSet) {
            if (isInRange(ip, cidr)) {
                set.add(cidr);
            }
        }
        return set;
    }


    public static String getIpAddr(HttpServletRequest request) {
        System.out.println(request);
        String ipAddress;
        try {
            ipAddress = request.getHeader("x-forwarded-for");
            if (ipAddress == null || ipAddress.length() == 0 || UNKNOWN.equalsIgnoreCase(ipAddress)) {
                ipAddress = request.getHeader("Proxy-Client-IP");
            }
            if (ipAddress == null || ipAddress.length() == 0 || UNKNOWN.equalsIgnoreCase(ipAddress)) {
                ipAddress = request.getHeader("WL-Proxy-Client-IP");
            }
            if (ipAddress == null || ipAddress.length() == 0 || UNKNOWN.equalsIgnoreCase(ipAddress)) {
                ipAddress = request.getRemoteAddr();
                if (LOCALHOST.equals(ipAddress)) {
                    InetAddress inet = null;
                    try {
                        inet = InetAddress.getLocalHost();
                    } catch (UnknownHostException e) {
                        e.printStackTrace();
                    }
                    ipAddress = inet.getHostAddress();
                }
            }
            // 对于通过多个代理的情况，第一个IP为客户端真实IP,多个IP按照','分割
            // "***.***.***.***".length()
            if (ipAddress != null && ipAddress.length() > 15) {
                if (ipAddress.indexOf(SEPARATOR) > 0) {
                    ipAddress = ipAddress.substring(0, ipAddress.indexOf(","));
                }
            }
        } catch (Exception e) {
            ipAddress = "";
        }
        return ipAddress;
    }
}
