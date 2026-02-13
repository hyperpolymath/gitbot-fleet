"""Setup configuration for Garment Sustainability Bot."""

from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name="gsbot",
    version="0.1.0",
    author="Garment Sustainability Bot Contributors",
    description="A Discord bot promoting sustainability in the garment and fashion industry",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/Hyperpolymath/gsbot",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Education",
        "Topic :: Education",
        "License :: OSI Approved :: Mozilla Public License 2.0 (MPL 2.0)",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
    ],
    python_requires=">=3.9",
    install_requires=[
        "discord.py>=2.3.2",
        "python-dotenv>=1.0.0",
        "sqlalchemy>=2.0.23",
        "alembic>=1.13.1",
        "pandas>=2.1.4",
        "numpy>=1.26.2",
        "aiohttp>=3.9.1",
        "requests>=2.31.0",
        "cachetools>=5.3.2",
        "pyyaml>=6.0.1",
        "colorlog>=6.8.0",
    ],
    extras_require={
        "dev": [
            "pytest>=7.4.3",
            "pytest-asyncio>=0.21.1",
            "pytest-cov>=4.1.0",
            "pytest-mock>=3.12.0",
            "black>=23.12.1",
            "flake8>=6.1.0",
            "mypy>=1.7.1",
            "pylint>=3.0.3",
        ],
    },
    entry_points={
        "console_scripts": [
            "gsbot=bot.main:main",
        ],
    },
)
