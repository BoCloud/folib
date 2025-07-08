package com.folib.metadata.extractor;

import org.redline_rpm.ReadableChannelWrapper;
import org.redline_rpm.Scanner;
import org.redline_rpm.header.Format;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.channels.Channels;

public class RpmFormatReader {

    public static Format read(String filePath) throws IOException {
        try (ReadableChannelWrapper channel = new ReadableChannelWrapper(Channels.newChannel(new FileInputStream(filePath)))) {
            Scanner scanner = new Scanner();
            return scanner.run(channel);
        }
    }
}
