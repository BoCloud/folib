/*
 * Folib - [新一代AI制品仓库]
 * Copyright (C) 2025 bocloud.com.cn <folib@beyondcent.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * 本程序是自由软件：您可依据GNU通用公共许可证（GPL-3.0+）条款重新发布和修改，
 * 但禁止任何形式的商业售卖行为（包括但不限于：直接销售、捆绑销售、云服务商用）。
 *
 * This program is distributed WITHOUT ANY WARRANTY.
 * Commercial sale of this software is expressly prohibited.
 *
 * For license details, see: https://www.gnu.org/licenses/gpl-3.0.html
 * 商业授权咨询请联系：folib@beyondcent.com
 */
package com.folib.common.encoding;

import java.util.BitSet;
import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.net.URLCodec;
import org.apache.http.HttpException;
import org.apache.http.util.EncodingUtils;

public class URI {
    protected static final BitSet percent = new BitSet(256);

    static {
        percent.set(37);
    }

    protected static final BitSet digit = new BitSet(256);

    static {
        int i;
        for (i = 48; i <= 57; i++) {
            digit.set(i);
        }
    }

    protected static final BitSet alpha = new BitSet(256);

    static {

        for (int i = 97; i <= 122; i++) {
            alpha.set(i);
        }
        for (int i = 65; i <= 90; i++) {
            alpha.set(i);
        }
    }

    protected static final BitSet alphanum = new BitSet(256);

    static {
        alphanum.or(alpha);
        alphanum.or(digit);
    }

    protected static final BitSet hex = new BitSet(256);

    static {
        hex.or(digit);
        for (int i = 97; i <= 102; i++) {
            hex.set(i);
        }
        for (int i = 65; i <= 70; i++) {
            hex.set(i);
        }
    }

    protected static final BitSet escaped = new BitSet(256);

    static {
        escaped.or(percent);
        escaped.or(hex);
    }

    protected static final BitSet mark = new BitSet(256);

    static {
        mark.set(45);
        mark.set(95);
        mark.set(46);
        mark.set(33);
        mark.set(126);
        mark.set(42);
        mark.set(39);
        mark.set(40);
        mark.set(41);
    }

    protected static final BitSet unreserved = new BitSet(256);

    static {
        unreserved.or(alphanum);
        unreserved.or(mark);
    }

    protected static final BitSet reserved = new BitSet(256);

    static {
        reserved.set(59);
        reserved.set(47);
        reserved.set(63);
        reserved.set(58);
        reserved.set(64);
        reserved.set(38);
        reserved.set(61);
        reserved.set(43);
        reserved.set(36);
        reserved.set(44);
    }

    protected static final BitSet uric = new BitSet(256);

    static {
        uric.or(reserved);
        uric.or(unreserved);
        uric.or(escaped);
    }

    protected static final BitSet fragment = uric;

    protected static final BitSet query = uric;

    protected static final BitSet pchar = new BitSet(256);

    static {
        pchar.or(unreserved);
        pchar.or(escaped);
        pchar.set(58);
        pchar.set(64);
        pchar.set(38);
        pchar.set(61);
        pchar.set(43);
        pchar.set(36);
        pchar.set(44);
    }

    protected static final BitSet param = pchar;

    protected static final BitSet segment = new BitSet(256);

    static {
        segment.or(pchar);
        segment.set(59);
        segment.or(param);
    }

    protected static final BitSet path_segments = new BitSet(256);

    static {
        path_segments.set(47);
        path_segments.or(segment);
    }

    protected static final BitSet abs_path = new BitSet(256);

    static {
        abs_path.set(47);
        abs_path.or(path_segments);
    }

    protected static final BitSet uric_no_slash = new BitSet(256);

    static {
        uric_no_slash.or(unreserved);
        uric_no_slash.or(escaped);
        uric_no_slash.set(59);
        uric_no_slash.set(63);
        uric_no_slash.set(59);
        uric_no_slash.set(64);
        uric_no_slash.set(38);
        uric_no_slash.set(61);
        uric_no_slash.set(43);
        uric_no_slash.set(36);
        uric_no_slash.set(44);
    }

    protected static final BitSet opaque_part = new BitSet(256);

    static {
        opaque_part.or(uric_no_slash);
        opaque_part.or(uric);
    }

    protected static final BitSet path = new BitSet(256);

    static {
        path.or(abs_path);
        path.or(opaque_part);
    }

    protected static final BitSet port = digit;

    protected static final BitSet IPv4address = new BitSet(256);

    static {
        IPv4address.or(digit);
        IPv4address.set(46);
    }

    protected static final BitSet IPv6address = new BitSet(256);

    static {
        IPv6address.or(hex);
        IPv6address.set(58);
        IPv6address.or(IPv4address);
    }

    protected static final BitSet IPv6reference = new BitSet(256);

    static {
        IPv6reference.set(91);
        IPv6reference.or(IPv6address);
        IPv6reference.set(93);
    }

    protected static final BitSet toplabel = new BitSet(256);

    static {
        toplabel.or(alphanum);
        toplabel.set(45);
    }

    protected static final BitSet hostname = new BitSet(256);

    static {
        hostname.or(toplabel);
        hostname.set(46);
    }

    protected static final BitSet host = new BitSet(256);

    static {
        host.or(hostname);
        host.or(IPv6reference);
    }

    protected static final BitSet hostport = new BitSet(256);

    static {
        hostport.or(host);
        hostport.set(58);
        hostport.or(port);
    }

    protected static final BitSet userinfo = new BitSet(256);

    static {
        userinfo.or(unreserved);
        userinfo.or(escaped);
        userinfo.set(59);
        userinfo.set(58);
        userinfo.set(38);
        userinfo.set(61);
        userinfo.set(43);
        userinfo.set(36);
        userinfo.set(44);
    }

    public static final BitSet within_userinfo = new BitSet(256);

    static {
        within_userinfo.or(userinfo);
        within_userinfo.clear(59);
        within_userinfo.clear(58);
        within_userinfo.clear(64);
        within_userinfo.clear(63);
        within_userinfo.clear(47);
    }

    protected static final BitSet server = new BitSet(256);

    static {
        server.or(userinfo);
        server.set(64);
        server.or(hostport);
    }

    protected static final BitSet reg_name = new BitSet(256);

    static {
        reg_name.or(unreserved);
        reg_name.or(escaped);
        reg_name.set(36);
        reg_name.set(44);
        reg_name.set(59);
        reg_name.set(58);
        reg_name.set(64);
        reg_name.set(38);
        reg_name.set(61);
        reg_name.set(43);
    }

    protected static final BitSet authority = new BitSet(256);

    static {
        authority.or(server);
        authority.or(reg_name);
    }

    protected static final BitSet scheme = new BitSet(256);

    static {
        scheme.or(alpha);
        scheme.or(digit);
        scheme.set(43);
        scheme.set(45);
        scheme.set(46);
    }

    protected static final BitSet rel_segment = new BitSet(256);

    static {
        rel_segment.or(unreserved);
        rel_segment.or(escaped);
        rel_segment.set(59);
        rel_segment.set(64);
        rel_segment.set(38);
        rel_segment.set(61);
        rel_segment.set(43);
        rel_segment.set(36);
        rel_segment.set(44);
    }

    protected static final BitSet rel_path = new BitSet(256);

    static {
        rel_path.or(rel_segment);
        rel_path.or(abs_path);
    }

    protected static final BitSet net_path = new BitSet(256);

    static {
        net_path.set(47);
        net_path.or(authority);
        net_path.or(abs_path);
    }

    protected static final BitSet hier_part = new BitSet(256);

    static {
        hier_part.or(net_path);
        hier_part.or(abs_path);
        hier_part.or(query);
    }

    protected static final BitSet relativeURI = new BitSet(256);

    static {
        relativeURI.or(net_path);
        relativeURI.or(abs_path);
        relativeURI.or(rel_path);
        relativeURI.or(query);
    }

    protected static final BitSet absoluteURI = new BitSet(256);

    static {
        absoluteURI.or(scheme);
        absoluteURI.set(58);
        absoluteURI.or(hier_part);
        absoluteURI.or(opaque_part);
    }

    protected static final BitSet URI_reference = new BitSet(256);

    static {
        URI_reference.or(absoluteURI);
        URI_reference.or(relativeURI);
        URI_reference.set(35);
        URI_reference.or(fragment);
    }

    public static final BitSet control = new BitSet(256);

    static {
        for (int i = 0; i <= 31; i++) {
            control.set(i);
        }
        control.set(127);
    }

    public static final BitSet space = new BitSet(256);

    static {
        space.set(32);
    }

    public static final BitSet delims = new BitSet(256);

    static {
        delims.set(60);
        delims.set(62);
        delims.set(35);
        delims.set(37);
        delims.set(34);
    }

    public static final BitSet unwise = new BitSet(256);

    static {
        unwise.set(123);
        unwise.set(125);
        unwise.set(124);
        unwise.set(92);
        unwise.set(94);
        unwise.set(91);
        unwise.set(93);
        unwise.set(96);
    }

    public static final BitSet disallowed_rel_path = new BitSet(256);

    static {
        disallowed_rel_path.or(uric);
        disallowed_rel_path.andNot(rel_path);
    }

    public static final BitSet disallowed_opaque_part = new BitSet(256);

    static {
        disallowed_opaque_part.or(uric);
        disallowed_opaque_part.andNot(opaque_part);
    }

    public static final BitSet allowed_authority = new BitSet(256);

    static {
        allowed_authority.or(authority);
        allowed_authority.clear(37);
    }

    public static final BitSet allowed_opaque_part = new BitSet(256);

    static {
        allowed_opaque_part.or(opaque_part);
        allowed_opaque_part.clear(37);
    }

    public static final BitSet allowed_reg_name = new BitSet(256);

    static {
        allowed_reg_name.or(reg_name);
        allowed_reg_name.clear(37);
    }

    public static final BitSet allowed_userinfo = new BitSet(256);

    static {
        allowed_userinfo.or(userinfo);
        allowed_userinfo.clear(37);
    }

    public static final BitSet allowed_within_userinfo = new BitSet(256);

    static {
        allowed_within_userinfo.or(within_userinfo);
        allowed_within_userinfo.clear(37);
    }

    public static final BitSet allowed_IPv6reference = new BitSet(256);

    static {
        allowed_IPv6reference.or(IPv6reference);
        allowed_IPv6reference.clear(91);
        allowed_IPv6reference.clear(93);
    }

    public static final BitSet allowed_host = new BitSet(256);

    static {
        allowed_host.or(hostname);
        allowed_host.or(allowed_IPv6reference);
    }

    public static final BitSet allowed_within_authority = new BitSet(256);

    static {
        allowed_within_authority.or(server);
        allowed_within_authority.or(reg_name);
        allowed_within_authority.clear(59);
        allowed_within_authority.clear(58);
        allowed_within_authority.clear(64);
        allowed_within_authority.clear(63);
        allowed_within_authority.clear(47);
    }

    public static final BitSet allowed_abs_path = new BitSet(256);

    static {
        allowed_abs_path.or(abs_path);
        allowed_abs_path.andNot(percent);
        allowed_abs_path.clear(43);
    }

    public static final BitSet allowed_rel_path = new BitSet(256);

    static {
        allowed_rel_path.or(rel_path);
        allowed_rel_path.clear(37);
        allowed_rel_path.clear(43);
    }

    public static final BitSet allowed_within_path = new BitSet(256);

    static {
        allowed_within_path.or(abs_path);
        allowed_within_path.clear(47);
        allowed_within_path.clear(59);
        allowed_within_path.clear(61);
        allowed_within_path.clear(63);
    }

    public static final BitSet allowed_query = new BitSet(256);

    static {
        allowed_query.or(uric);
        allowed_query.clear(37);
    }

    public static final BitSet allowed_within_query = new BitSet(256);

    static {
        allowed_within_query.or(allowed_query);
        allowed_within_query.andNot(reserved);
    }

    public static final BitSet allowed_fragment = new BitSet(256);

    static {
        allowed_fragment.or(uric);
        allowed_fragment.clear(37);
    }

    protected static char[] encode(String original, BitSet allowed, String charset) throws HttpException {
        if (original == null) {
            throw new IllegalArgumentException("Original string may not be null");
        }
        if (allowed == null) {
            throw new IllegalArgumentException("Allowed bitset may not be null");
        }
        byte[] rawdata = URLCodec.encodeUrl(allowed, EncodingUtils.getBytes(original, charset));
        return EncodingUtils.getAsciiString(rawdata).toCharArray();
    }

    public static String decode(char[] component, String charset) throws HttpException {
        if (component == null) {
            throw new IllegalArgumentException("Component array of chars may not be null");
        }
        return decode(new String(component), charset);
    }

    public static String decode(String component, String charset) throws HttpException {
        if (component == null) {
            throw new IllegalArgumentException("Component array of chars may not be null");
        }
        byte[] rawdata = null;
        try {
            rawdata = URLCodec.decodeUrl(EncodingUtils.getAsciiBytes(component));
        } catch (DecoderException e) {
            throw new HttpException(e.getMessage());
        }
        return EncodingUtils.getString(rawdata, charset);
    }
}

