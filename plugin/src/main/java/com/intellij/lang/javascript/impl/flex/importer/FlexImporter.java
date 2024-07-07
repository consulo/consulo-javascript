/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.impl.flex.importer;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import consulo.language.psi.stub.StubElement;
import jakarta.annotation.Nonnull;
import org.jetbrains.annotations.NonNls;

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

	private static void processFlexByteCode(@Nonnull final InputStream in, @Nonnull FlexByteCodeInformationProcessor processor) throws IOException
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
