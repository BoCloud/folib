package com.folib.nuget.indexer.symbols.pdb;

import com.folib.nuget.indexer.symbols.exception.PDBParseException;
import com.folib.nuget.indexer.symbols.stream.MsfStream;
import jakarta.annotation.Nullable;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;



@Getter
@Slf4j
public class MicrosoftPdbV7Parser extends PdbParser{
    private static final int PAGE_SIZE_OFFSET = 32;

    private final List<MsfStream> streamDescriptorTable = new ArrayList();
    private int pageSize;
    private int log2PageSize;

    public MicrosoftPdbV7Parser(byte[] fileBytes) {
        super(fileBytes);
    }

    private void parseRootStream() {
        this.pageSize = this.parseInt(32);
        this.log2PageSize = (int)(Math.log((double)this.pageSize) / Math.log((double)2.0F));
        int directoryStreamMapPageNumber = this.parseInt(52);
        int directoryStreamFirstPageNumber = this.parseInt(this.getPageOffset(directoryStreamMapPageNumber));
        this.initStreamTable(directoryStreamFirstPageNumber);
    }

    private void initStreamTable(int directoryStreamFirstPage) {
        int numOfStreams = this.parseInt(this.getPageOffset(directoryStreamFirstPage));
        int index = this.getPageOffset(directoryStreamFirstPage) + 4;

        for(int streamNumber = 0; streamNumber < numOfStreams; ++streamNumber) {
            int streamLength = this.parseInt(index);
            this.streamDescriptorTable.add(new MsfStream(streamLength));
            index += 4;
        }

        for(int streamNumber = 0; streamNumber < numOfStreams; ++streamNumber) {
            MsfStream stream = (MsfStream)this.streamDescriptorTable.get(streamNumber);
            int length = stream.getStreamLength();
            int numOfPagesForStream = PdbUtils.floorDivisionWithLog2Divisor(length, this.log2PageSize);

            for(int i = 0; i < numOfPagesForStream; ++i) {
                int page = this.parseInt(index);
                stream.addPage(page);
                index += 4;
            }
        }

    }

    private int getPageOffset(int pageNumber) {
        return pageNumber * this.pageSize;
    }

    @Nullable
    public PdbGuid parsePdbAndExtractGuid() {
        try {
            this.parseRootStream();
            MsfStream stream = this.getStream1();
            if (stream == null) {
                log.warn("Could not find stream#1 in stream table");
                return null;
            } else {
                int streamPage = (Integer)stream.getPageList().get(0);
                int streamOffset = this.getPageOffset(streamPage);
                this.pdbAge = this.parseInt(streamOffset + 8);
                byte[] guidBytes = Arrays.copyOfRange(this.FILE_BYTES, streamOffset + 12, streamOffset + 28);
                this.guid = new PdbGuid(guidBytes);
                return this.guid;
            }
        } catch (Exception e) {
            log.error("Could not parse Microsoft MSF 7.00 PDB because of the following: {}", e.getMessage());
            log.debug("Could not parse Microsoft MSF 7.00 PDB because of the following: ", e);
            throw new PDBParseException(e);
        }
    }

    private MsfStream getStream1() {
        return (MsfStream)this.streamDescriptorTable.get(1);
    }
}
