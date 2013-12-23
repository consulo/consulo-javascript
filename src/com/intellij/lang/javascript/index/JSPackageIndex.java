package com.intellij.lang.javascript.index;

import gnu.trove.THashMap;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.idea.LoggerFactory;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.JSResolveHelper;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import com.intellij.lang.javascript.psi.stubs.JSFunctionExpressionStub;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.lang.javascript.psi.stubs.JSNamespaceDeclarationStub;
import com.intellij.lang.javascript.psi.stubs.JSPackageStatementStub;
import com.intellij.lang.javascript.psi.stubs.JSParameterStub;
import com.intellij.lang.javascript.psi.stubs.JSQualifiedStub;
import com.intellij.lang.javascript.psi.stubs.JSVarStatementStub;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.BinaryFileStubBuilder;
import com.intellij.psi.stubs.BinaryFileStubBuilders;
import com.intellij.psi.stubs.PsiFileStub;
import com.intellij.psi.stubs.Stub;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubTree;
import com.intellij.util.Processor;
import com.intellij.util.SmartList;
import com.intellij.util.indexing.CustomImplementationFileBasedIndexExtension;
import com.intellij.util.indexing.DataIndexer;
import com.intellij.util.indexing.FileBasedIndex;
import com.intellij.util.indexing.FileBasedIndexExtension;
import com.intellij.util.indexing.FileContent;
import com.intellij.util.indexing.ID;
import com.intellij.util.indexing.IndexStorage;
import com.intellij.util.indexing.MapReduceIndex;
import com.intellij.util.indexing.UpdatableIndex;
import com.intellij.util.io.DataExternalizer;
import com.intellij.util.io.EnumeratorStringDescriptor;
import com.intellij.util.io.IOUtil;
import com.intellij.util.io.KeyDescriptor;
import com.intellij.util.text.StringTokenizer;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: 10.03.2009
 * Time: 19:11:15
 * To change this template use File | Settings | File Templates.
 */
public class JSPackageIndex extends CustomImplementationFileBasedIndexExtension<String, List<JSPackageIndexInfo>, FileContent>
{
	private static final int myVersion = 6;

	public static final ID<String, List<JSPackageIndexInfo>> INDEX_ID = new ID<String, List<JSPackageIndexInfo>>("js.package.index")
	{
	};

	private final DataIndexer<String, List<JSPackageIndexInfo>, FileContent> myIndexer = new DataIndexer<String, List<JSPackageIndexInfo>,
			FileContent>()
	{
		@Override
		@NotNull
		public Map<String, List<JSPackageIndexInfo>> map(FileContent inputData)
		{
			final THashMap<String, List<JSPackageIndexInfo>> map = new THashMap<String, List<JSPackageIndexInfo>>();

			StubTree tree;

			if(inputData.getFileType().isBinary())
			{
				BinaryFileStubBuilder binaryFileStubBuilder = BinaryFileStubBuilders.INSTANCE.forFileType(inputData.getFileType());
				if(binaryFileStubBuilder == null)
				{
					return Collections.emptyMap();
				}

				Stub stubElement;

				try
				{
					stubElement = binaryFileStubBuilder.buildStubTree(inputData);
				}
				catch(Exception e)
				{
					stubElement = null;
					LoggerFactory.getInstance().getLoggerInstance(getClass().getName()).error("Corrupted zip file", e);
				}
				if(stubElement == null)
				{
					return Collections.emptyMap();
				}

				tree = new StubTree((PsiFileStub) stubElement);
			}
			else
			{
				PsiFile psiFile = inputData.getPsiFile();
				if(!(psiFile instanceof JSFile))
				{
					return Collections.emptyMap();
				}

				tree = ((PsiFileImpl) psiFile).getStubTree();
				if(tree == null)
				{
					tree = ((PsiFileImpl) psiFile).calcStubTree();
				}
			}

			for(StubElement e : tree.getPlainList())
			{
				JSQualifiedStub element = null;
				if(e instanceof JSQualifiedStub)
				{
					element = (JSQualifiedStub) e;
				}
				if(e instanceof JSParameterStub)
				{
					element = null;
				}
				else if(e instanceof JSFunctionExpressionStub)
				{
					element = null;
				}
				else if(e instanceof JSFunctionStub || e instanceof JSVariableStub)
				{
					StubElement parentStub = e.getParentStub();
					if(parentStub instanceof JSVarStatementStub)
					{
						parentStub = parentStub.getParentStub();
					}
					if(!(parentStub instanceof JSPackageStatementStub) && !(parentStub instanceof PsiFileStub))
					{
						element = null;
					}
				}

				if(element != null)
				{
					indexQualifiedElement(element, map);
				}
			}

			return map;
		}

		final StringBuilder builder = new StringBuilder();

		private void indexQualifiedElement(JSQualifiedStub element, Map<String, List<JSPackageIndexInfo>> map)
		{
			String qName = element.getQualifiedName();
			if(qName != null)
			{
				JSPackageIndexInfo.Kind kind = element instanceof JSClassStub || element instanceof JSNamespaceDeclarationStub ? JSPackageIndexInfo.Kind.CLASS :
						element instanceof JSFunctionStub ? JSPackageIndexInfo.Kind.FUNCTION : element instanceof JSVariableStub ? JSPackageIndexInfo.Kind.VARIABLE :
								JSPackageIndexInfo.Kind.PACKAGE;
				StringTokenizer tokenizer = new StringTokenizer(qName, ".");
				builder.setLength(0);
				String el = null;

				while(tokenizer.hasMoreElements())
				{
					if(el != null)
					{
						if(builder.length() != 0)
						{
							builder.append('.');
						}
						builder.append(el);
					}

					el = tokenizer.nextElement();
					JSPackageIndexInfo.Kind currentKind = tokenizer.hasMoreElements() ? JSPackageIndexInfo.Kind.PACKAGE : kind;

					String key = builder.length() != 0 ? builder.toString() : "";
					List<JSPackageIndexInfo> infoList = map.get(key);
					boolean toAdd = true;

					if(infoList == null)
					{
						map.put(key, infoList = new SmartList<JSPackageIndexInfo>());
					}
					else
					{
						for(JSPackageIndexInfo info : infoList)
						{
							if(info.isEquivalentTo(el, currentKind))
							{
								toAdd = false;
								break;
							}
						}
					}

					if(toAdd)
					{
						infoList.add(new JSPackageIndexInfo(el, currentKind));
					}
				}
			}
		}
	};

	public static String buildQualifiedName(String packageName, String className)
	{
		return (packageName != null && packageName.length() > 0 ? packageName + "." : "") + className;
	}

	private final FileBasedIndex.InputFilter myInputFilter = new FileBasedIndex.InputFilter()
	{
		final FileType swfFileType = FileTypeManager.getInstance().getFileTypeByExtension("swf");

		@Override
		public boolean acceptInput(final Project project, final VirtualFile file)
		{
			FileType type = file.getFileType();
			return type == JavaScriptSupportLoader.JAVASCRIPT || type == swfFileType;
		}
	};

	private final KeyDescriptor<String> myKeyDescriptor = new EnumeratorStringDescriptor();
	private final DataExternalizer<List<JSPackageIndexInfo>> myDataExternalizer = new DataExternalizer<List<JSPackageIndexInfo>>()
	{
		private final byte[] buffer = IOUtil.allocReadWriteUTFBuffer();
		private final JSPackageIndexInfo.Kind[] kinds = JSPackageIndexInfo.Kind.values();

		@Override
		public void save(DataOutput out, List<JSPackageIndexInfo> value) throws IOException
		{
			out.writeInt(value.size());
			for(JSPackageIndexInfo s : value)
			{
				out.writeByte(s.kind.ordinal());
				IOUtil.writeUTFFast(buffer, out, s.name);
			}
		}

		@Override
		public List<JSPackageIndexInfo> read(DataInput in) throws IOException
		{
			int size = in.readInt();
			List<JSPackageIndexInfo> strings = new ArrayList<JSPackageIndexInfo>(size);

			while(size-- > 0)
			{
				JSPackageIndexInfo.Kind kind = kinds[in.readByte()];
				String s = IOUtil.readUTFFast(buffer, in);
				strings.add(new JSPackageIndexInfo(s, kind));
			}
			return strings;
		}
	};

	@Override
	public ID<String, List<JSPackageIndexInfo>> getName()
	{
		return INDEX_ID;
	}

	@Override
	public DataIndexer<String, List<JSPackageIndexInfo>, FileContent> getIndexer()
	{
		return myIndexer;
	}

	@Override
	public KeyDescriptor<String> getKeyDescriptor()
	{
		return myKeyDescriptor;
	}

	@Override
	public DataExternalizer<List<JSPackageIndexInfo>> getValueExternalizer()
	{
		return myDataExternalizer;
	}

	@Override
	public FileBasedIndex.InputFilter getInputFilter()
	{
		return myInputFilter;
	}

	@Override
	public boolean dependsOnFileContent()
	{
		return true;
	}

	@Override
	public int getVersion()
	{
		return myVersion;
	}

	@Override
	public int getCacheSize()
	{
		return FileBasedIndexExtension.DEFAULT_CACHE_SIZE;
	}

	@Override
	public UpdatableIndex<String, List<JSPackageIndexInfo>, FileContent> createIndexImplementation(ID<String, List<JSPackageIndexInfo>> indexId,
			FileBasedIndex owner, IndexStorage<String, List<JSPackageIndexInfo>> indexStorage)
	{
		return new MapReduceIndex<String, List<JSPackageIndexInfo>, FileContent>(indexId, myIndexer, indexStorage);
	}

	public interface PackageElementsProcessor
	{
		boolean process(VirtualFile file, String name, JSPackageIndexInfo.Kind kind);
	}

	public static boolean processElementsInScope(@NotNull final String packageName, @Nullable final String targetName,
			final PackageElementsProcessor processor, final GlobalSearchScope searchScope, Project project)
	{
		FileBasedIndex.ValueProcessor<List<JSPackageIndexInfo>> indexProcessor = new FileBasedIndex.ValueProcessor<List<JSPackageIndexInfo>>()
		{
			@Override
			public boolean process(VirtualFile file, List<JSPackageIndexInfo> members)
			{
				if(targetName != null)
				{
					return processor.process(file, targetName, JSPackageIndexInfo.Kind.PACKAGE);
				}
				for(JSPackageIndexInfo className : members)
				{
					if(!processor.process(file, className.name, className.kind))
					{
						return false;
					}
				}

				return true;
			}
		};

		boolean b;

		if(targetName == null)
		{
			b = FileBasedIndex.getInstance().processValues(INDEX_ID, packageName, null, indexProcessor, searchScope);
		}
		else
		{
			String target = buildQualifiedName(packageName, targetName);
			b = FileBasedIndex.getInstance().processValues(INDEX_ID, target, null, indexProcessor, searchScope);
		}

		if(b)
		{
			Processor<VirtualFile> vfileProcessor = new Processor<VirtualFile>()
			{
				@Override
				public boolean process(VirtualFile virtualFile)
				{
					JSPackageIndexInfo.Kind kind = null;

					if(virtualFile.isDirectory())
					{
						kind = JSPackageIndexInfo.Kind.PACKAGE;
					}
					else if(JavaScriptSupportLoader.isFlexMxmFile(virtualFile))
					{
						kind = JSPackageIndexInfo.Kind.CLASS;
					}
					if(kind == null)
					{
						return true;
					}

					return processor.process(virtualFile, virtualFile.getNameWithoutExtension(), kind);
				}
			};
			for(JSResolveHelper helper : Extensions.getExtensions(JSResolveHelper.EP_NAME))
			{
				if(!helper.processPackage(packageName, targetName, vfileProcessor, searchScope, project))
				{
					return true;
				}
			}
		}
		return b;
	}
}
