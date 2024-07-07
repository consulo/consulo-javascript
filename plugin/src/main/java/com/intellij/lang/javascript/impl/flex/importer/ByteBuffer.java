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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.zip.InflaterInputStream;

import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 *         Date: Oct 20, 2008
 *         Time: 7:02:53 PM
 */
class ByteBuffer
{
	private byte[] bytes;
	private int position;
	private boolean littleEndian;

	void read(@Nonnull InputStream inputStream) throws IOException
	{
		try
		{
			bytes = readStream(inputStream);
		}
		finally
		{
			if(inputStream != null)
			{
				inputStream.close();
			}
		}
	}

	void setLittleEndian()
	{
		littleEndian = true;
	}

	int readInt()
	{
		int result;
		if(littleEndian)
		{
			result = (((bytes[position + 3] & 0xFF) << 8 | (bytes[position + 2] & 0xFF)) << 16) + ((bytes[position + 1] & 0xFF) << 8) | (bytes[position] &
					0xFF);
		}
		else
		{
			result = (((bytes[position] & 0xFF) << 8 | (bytes[position + 1] & 0xFF)) << 16) + ((bytes[position + 2] & 0xFF) << 8) | (bytes[position + 3] &
					0xFF);
		}
		position += 4;
		return result;
	}

	public int readUnsignedInt()
	{
		return readInt();
	}

	public void setPosition(final int i)
	{
		position = i;
	}

	public int bytesSize()
	{
		return bytes.length;
	}

	public void uncompress() throws IOException
	{
		final InflaterInputStream zipInputStream = new InflaterInputStream(new ByteArrayInputStream(bytes));

		bytes = readStream(zipInputStream);

		zipInputStream.close();
	}

	private static byte[] readStream(final InputStream zipInputStream) throws IOException
	{
		final byte[] buf = new byte[8192];
		byte[] result = new byte[8192];
		int total = 0;

		while(true)
		{
			int read = zipInputStream.read(buf);
			if(read == -1)
			{
				break;
			}
			if(total + read >= result.length)
			{
				byte[] newresult = new byte[result.length * 2];
				System.arraycopy(result, 0, newresult, 0, total);
				result = newresult;
			}

			System.arraycopy(buf, 0, result, total, read);
			total += read;
		}

		final byte[] realResult = new byte[total];
		System.arraycopy(result, 0, realResult, 0, total);
		return realResult;
	}

	public int readUnsignedByte()
	{
		return bytes[position++] & 0xFF;
	}

	public int readByte()
	{
		return bytes[position++];
	}

	public int readUnsignedShort()
	{
		int result;
		if(littleEndian)
		{
			result = (bytes[position + 1] & 0xFF) << 8 | (bytes[position] & 0xFF);
		}
		else
		{
			result = (bytes[position] & 0xFF) << 8 | (bytes[position + 1] & 0xFF);
		}
		position += 2;
		return result;
	}

	public void readBytes(ByteBuffer data2, int length)
	{
		data2.bytes = new byte[length];
		System.arraycopy(bytes, position, data2.bytes, 0, length);
		position += length;
	}

	public boolean eof()
	{
		return position >= bytes.length;
	}

	public String readUTFBytes(int i)
	{
		try
		{
			final byte[] buf = new byte[i];
			while(i > 0)
			{
				buf[buf.length - i] = (byte) readByte();
				--i;
			}
			return new String(buf, "utf-8");
		}
		catch(UnsupportedEncodingException e)
		{
			throw new RuntimeException(e);
		}
	}

	public double readDouble()
	{
		int first = readInt();
		int second = readInt();
		return Double.longBitsToDouble(((long) second << 32) | first);
	}

	public int readU32()
	{
		int result = readUnsignedByte();
		if((result & 0x00000080) == 0)
		{
			return result;
		}
		result = result & 0x0000007f | readUnsignedByte() << 7;
		if((result & 0x00004000) == 0)
		{
			return result;
		}
		result = result & 0x00003fff | readUnsignedByte() << 14;
		if((result & 0x00200000) == 0)
		{
			return result;
		}
		result = result & 0x001fffff | readUnsignedByte() << 21;
		if((result & 0x10000000) == 0)
		{
			return result;
		}
		return result & 0x0fffffff | readUnsignedByte() << 28;
	}

	public byte getByte(int i)
	{
		return bytes[i];
	}

	public int getPosition()
	{
		return position;
	}

	public void incPosition(final int length)
	{
		position += length;
	}
}
