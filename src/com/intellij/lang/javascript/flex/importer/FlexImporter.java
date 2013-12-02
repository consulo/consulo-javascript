package com.intellij.lang.javascript.flex.importer;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.psi.stubs.StubElement;

/**
 * Produced from abcdump.as
 */
public class FlexImporter
{
	public static void main(String[] args) throws IOException
	{
		if(args.length < 1)
		{
			System.out.print("FlexImporter\nusage:\nFlexImporter <filename>");
		}
		else
		{
			long started = System.currentTimeMillis();

			for(String file : args)
			{
				try
				{
					String result = dumpContentsFromStream(new BufferedInputStream(new FileInputStream(file)), true);

					saveStringAsFile(result, file + ".il");
				}
				finally
				{
					long total = System.currentTimeMillis() - started;
					System.out.println("File created... " + total + "ms");
				}
			}
		}
	}

	private static void saveStringAsFile(final String result, final String fileName) throws IOException
	{
		final FileOutputStream fileOutputStream = new FileOutputStream(fileName);
		try
		{
			fileOutputStream.write(result.getBytes());
		}
		finally
		{
			fileOutputStream.close();
		}
	}

	public static String dumpContentsFromStream(final InputStream in, boolean _dumpCode) throws IOException
	{
		final AbstractDumpProcessor abcDumper = new AbcDumper(_dumpCode);
		processFlexByteCode(in, abcDumper);
		return abcDumper.getResult();
	}

	@NonNls
	public static String buildInterfaceFromStream(final InputStream in)
	{
		try
		{
			final AbstractDumpProcessor abcDumper = new AS3InterfaceDumper();
			processFlexByteCode(in, abcDumper);
			final String s = abcDumper.getResult();
			//saveStringAsFile(s, File.createTempFile("fleximport", ".as").getPath());
			return s;
		}
		catch(IOException ex)
		{
			return "/* " + ex.getLocalizedMessage() + " */";
		}
		catch(ArrayIndexOutOfBoundsException ex)
		{
			return "/* Invalid format */";
		}
	}

	@NonNls
	public static void buildStubsInterfaceFromStream(final InputStream in, final StubElement parent) throws Exception
	{
		processFlexByteCode(in, new AS3InterfaceStubDumper(parent));
	}

	private static void processFlexByteCode(@NotNull final InputStream in, @NotNull FlexByteCodeInformationProcessor processor) throws IOException
	{
		ByteBuffer data = new ByteBuffer();
		data.read(in);
		data.setLittleEndian();
		int version = data.readUnsignedInt();

		switch(version)
		{
			case 46 << 16 | 14:
			case 46 << 16 | 15:
			case 46 << 16 | 16:
				Abc abc = new Abc(data, processor);
				abc.dump("");
				break;
			case 67 | 87 << 8 | 83 << 16 | 10 << 24: // needed for airglobal.swc
			case 67 | 87 << 8 | 83 << 16 | 9 << 24: // SWC9
			case 67 | 87 << 8 | 83 << 16 | 8 << 24: // SWC8
			case 67 | 87 << 8 | 83 << 16 | 7 << 24: // SWC7
			case 67 | 87 << 8 | 83 << 16 | 6 << 24: // SWC6
				final int delta = 8;
				data.setPosition(delta);
				ByteBuffer udata = new ByteBuffer();
				udata.setLittleEndian();
				data.readBytes(udata, data.bytesSize() - delta);
				int csize = udata.bytesSize();
				udata.uncompress();
				processor.dumpStat("decompressed swf " + csize + " -> " + udata.bytesSize() + "\n");
				udata.setPosition(0);
				new Swf(udata, processor);
				break;
			case 70 | 87 << 8 | 83 << 16 | 9 << 24: // SWC9
			case 70 | 87 << 8 | 83 << 16 | 8 << 24: // SWC8
			case 70 | 87 << 8 | 83 << 16 | 7 << 24: // SWC7
			case 70 | 87 << 8 | 83 << 16 | 6 << 24: // SWC6
			case 70 | 87 << 8 | 83 << 16 | 5 << 24: // SWC5
			case 70 | 87 << 8 | 83 << 16 | 4 << 24: // SWC4
				data.setPosition(8); // skip header and length
				new Swf(data, processor);
				break;
			default:
				processor.hasError("unknown format " + version + "\n");
				break;
		}
		in.close();
	}

}
