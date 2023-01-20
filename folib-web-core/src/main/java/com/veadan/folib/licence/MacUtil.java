package com.veadan.folib.licence;


import cn.hutool.core.net.NetUtil;

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Created by paul on 2018/6/29.
 */
public class MacUtil {

//    private static final int SPLITLENGTH = 4;

    public static void main(String[] args) throws Exception {
        System.out.println("getMachineCode = " + getMachineCode());
    }

    public static String getMachineCode() throws Exception {
        Set<String> result = new HashSet<>();
        InetAddress inetAddress = InetAddress.getLocalHost();
        //第二种方式：利用hutool工具类中的封装方法获取本机mac地址
        String mac = NetUtil.getMacAddress(inetAddress);
        result.add(mac);
        Properties props = System.getProperties();
        String javaVersion = props.getProperty("java.version");
        result.add(javaVersion);
        String javaVMVersion = props.getProperty("java.vm.version");
        result.add(javaVMVersion);
        String osVersion = props.getProperty("os.version");
        result.add(osVersion);
        String code = Encrpt.GetMD5Code(result.toString());
        return getSplitString(code, "-", 4);
    }


    public static String getSplitString(String str, String split, int length) {
        int len = str.length();
        StringBuilder temp = new StringBuilder();
        for (int i = 0; i < len; i++) {
            if (i % length == 0 && i > 0) {
                temp.append(split);
            }
            temp.append(str.charAt(i));
        }
        String[] attrs = temp.toString().split(split);
        StringBuilder finalMachineCode = new StringBuilder();
        for (String attr : attrs) {
            if (attr.length() == length) {
                finalMachineCode.append(attr).append(split);
            }
        }
        String result = finalMachineCode.toString().substring(0,
                finalMachineCode.toString().length() - 1);
        return result;
    }

    //    public static String getSplitString(String str) {
//        return getSplitString(str, "-", SPLITLENGTH);
//    }

//    private static String bytesToHexString(byte[] src) {
//        StringBuilder stringBuilder = new StringBuilder("");
//        if (src == null || src.length <= 0) {
//            return null;
//        }
//        for (int i = 0; i < src.length; i++) {
//            int v = src[i] & 0xFF;
//            String hv = Integer.toHexString(v);
//            if (hv.length() < 2) {
//                stringBuilder.append(0);
//            }
//            stringBuilder.append(hv);
//        }
//        return stringBuilder.toString();
//    }


//    public static String getMacId() {
//        String macId = "";
//        InetAddress ip = null;
//        NetworkInterface ni = null;
//        try {
//            boolean bFindIP = false;
//            Enumeration<NetworkInterface> netInterfaces = (Enumeration<NetworkInterface>) NetworkInterface
//                    .getNetworkInterfaces();
//            while (netInterfaces.hasMoreElements()) {
//                if (bFindIP) {
//                    break;
//                }
//                ni = (NetworkInterface) netInterfaces
//                        .nextElement();
//                Enumeration<InetAddress> ips = ni.getInetAddresses();
//                while (ips.hasMoreElements()) {
//                    ip = (InetAddress) ips.nextElement();
//                    if (!ip.isLoopbackAddress() // 非127.0.0.1
//                            && ip.getHostAddress().matches(
//                            "(\\d{1,3}\\.){3}\\d{1,3}")) {
//                        bFindIP = true;
//                        break;
//                    }
//                }
//            }
//        } catch (Exception e) {
//            e.printStackTrace();
//        }
//        if (null != ip) {
//            try {
//                macId = getMacFromBytes(ni.getHardwareAddress());
//            } catch (SocketException e) {
//                e.printStackTrace();
//            }
//        }
//        return macId;
//    }

//    private static String getMacFromBytes(byte[] bytes) {
//        StringBuffer mac = new StringBuffer();
//        byte currentByte;
//        boolean first = false;
//        for (byte b : bytes) {
//            if (first) {
//                mac.append("-");
//            }
//            currentByte = (byte) ((b & 240) >> 4);
//            mac.append(Integer.toHexString(currentByte));
//            currentByte = (byte) (b & 15);
//            mac.append(Integer.toHexString(currentByte));
//            first = true;
//        }
//        return mac.toString().toUpperCase();
//    }


}
